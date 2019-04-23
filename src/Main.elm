port module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, input, span, textarea, br, p)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List exposing (map,head,tail,reverse)
import Debug exposing (toString)
import Json.Decode as Decode exposing (Decoder, field, bool, int, string)
import String exposing (split)

import Tokenizer
import Parser
import Environment exposing (Env, lookup)
import Evaluate
import Types exposing (..)
import Typecheck exposing (CheckResult(..), typecheck, checkResultToString, typeToString)


-- MAIN
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Model =
  { content : String
  , vars       : List Var
  , renderTreeInfos : List RenderTreeInfo
  , errorMsg : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { content = ""
    , vars = []
    , renderTreeInfos = []
    , errorMsg = ""
    }
  , Cmd.none )




-- UPDATE

type Msg
  = Change String | IncDepth Int | DecDepth Int | GotAsts (Result String (List (String, Term)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      let
        lines = split "\n" newContent
      in
        ({ model | content = newContent }, parseLines lines)

    IncDepth id ->
      let
        newRTs = filterUpdate (\x -> x.id == id) (\x -> {x | depth = x.depth + 1}) model.renderTreeInfos
      in
        ({model | renderTreeInfos = newRTs} , Cmd.none )

    DecDepth id ->
      let
        newRTs = filterUpdate (\x -> x.id == id) (\x -> {x | depth = x.depth - 1}) model.renderTreeInfos
      in
        ({model | renderTreeInfos = newRTs} , Cmd.none )
    
    GotAsts r ->
      case r of
        Ok ts ->
          let
            vs = map (\(n,t) -> {name=n, term=t}) ts
            ris = genRenderInfos 3 vs
          in
            ({ model | vars = vs, renderTreeInfos = ris }
            , Cmd.none)

        Err e ->
          ({ model | errorMsg = e }, Cmd.none)


-- runs update function on items passing the filter function
filterUpdate : (a -> Bool) -> (a -> a) -> List a -> List a
filterUpdate cond upd xs =
  map
    (\x ->
      if cond x then
        upd x
      else
        x
    )
    xs


{-
Generates a list of render infos.  This function exists mostly so that render
infos can recieve ids.
-}
genRenderInfos : Int -> List Var -> List RenderTreeInfo
genRenderInfos depth vars =
  List.indexedMap
    ( \i var ->
        { id = i
        , var = var
        , depth = depth
        } )
    vars


-- PORTS
port parseLines : List String -> Cmd a
port gotAst : (Decode.Value -> msg) -> Sub msg


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  gotAst (decodeAsts >> GotAsts)


decodeAsts : Decode.Value -> Result String (List (String, Term))
decodeAsts v =
  case Decode.decodeValue (Decode.list assignDecoder) v of
    Ok t -> Ok t
    Err e -> Err (Decode.errorToString e)

assignDecoder : Decoder (String, Term)
assignDecoder = 
  Decode.map2 (\id t -> (id, t))
    (field "identifier" string)
    (field "expression" exprDecoder)

exprDecoder : Decoder Term
exprDecoder =
  field "type" string
    |> Decode.andThen exprSwitch

exprSwitch : String -> Decoder Term
exprSwitch s =
  case s of
    "INT"  -> intDecoder
    "BOOL" -> boolDecoder
    "ID"   -> idDecoder
    "ADD_EXPR" -> addDecoder
    "SUBT_EXPR" -> subtDecoder
    "MULT_EXPR" -> multDecoder
    "AND_EXPR" -> andDecoder
    "OR_EXPR" -> orDecoder
    "EQ_EXPR" -> eqDecoder
    _      -> Decode.fail ("unrecognized type: " ++ s)

binDecoder : (Term -> Term -> Term) -> Decoder Term
binDecoder comb =
  field "children" (Decode.list exprDecoder)
    |> Decode.andThen
      (\ts ->
        case reverse ts of
          t1::t2::tr ->
            Decode.succeed (binCombiner comb t1 ([t2] ++ tr))
          _ ->
            Decode.fail "binary expression has fewer than 2 children"
      )

{-
This function needs at least one item in list to return a Term; the Elm
community seems to like this approach of passing a 'first' input followed
by the remaineder to ensure that at least one input is recieved.  I'm still
not sure how I feel about this.
-}
binCombiner : (Term -> Term -> Term) -> Term -> List Term -> Term
binCombiner comb first ts =
  case ts of
    [] ->
      first
    t::tr ->
      comb (binCombiner comb t tr) first

addDecoder = binDecoder (\t1 t2 -> Plus t1 t2)
subtDecoder = binDecoder (\t1 t2 -> Minus t1 t2)
multDecoder = binDecoder (\t1 t2 -> Times t1 t2)
andDecoder = binDecoder (\t1 t2 -> And t1 t2)
orDecoder = binDecoder (\t1 t2 -> Or t1 t2)

eqDecoder : Decoder Term
eqDecoder =
  Decode.map2 (\l r -> (l, r))
    (field "lhs" exprDecoder)
    (field "rhs" exprDecoder)
    |> Decode.andThen
      (\(l, r) -> Decode.succeed (Eq l r))

intDecoder : Decoder Term
intDecoder =
  field "value" int
    |> Decode.andThen
      (\i -> Decode.succeed (CTerm (CInt i)))

boolDecoder : Decoder Term
boolDecoder =
  field "value" bool
    |> Decode.andThen
      (\b -> Decode.succeed (CTerm (CBool b)))

idDecoder : Decoder Term
idDecoder =
  field "value" string
    |> Decode.andThen
      (\i -> Decode.succeed (VTerm i))

-- VIEW

view : Model -> Document Msg
view model =
 { title = "TreeScript"
  , body =
    [
      Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
      , p [] [text model.errorMsg]
      , div [class "tokenizer-parser-title-container"]
        [
          div [class "textarea-container"]
          [
            h3 [ class "css-title" ] [text "Input Program:"]
            , textarea [ rows 10, cols 50, placeholder "Text to render", value model.content, onInput Change ] []
          ]
          , div [class "tokenizer-parser-container"]
          [
            h3 [ class "css-title" ] [text "Parse Tree:"]
            , div [class "expression-builder"] (printPT model.vars)
          ]
        ]
      , div [ class "flexs-container" ]
      [
         div [class "tree-title-container"]
         [
            h3 [class "css-title"] [text "Derivation Tree:"]
          , div [class "trees-container"] (printRT model.vars model.renderTreeInfos)
         ]
      ]
    ]
 }

printTknsLBL : List (List Token) -> List (Html Msg)
printTknsLBL tkns =
  case tkns of
    []-> [div [class "tkns-div"] [text ""]]
    (l::ls) -> [div [class "tkns-div"] [text (Tokenizer.tokenizePrint l)]] ++ (printTknsLBL ls)

renderSummary : Env -> RenderTree -> Html Msg
renderSummary envr (Node rNode _) =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ Evaluate.valToString (Evaluate.eval envr rNode.term) )
  ]


renderTree : Env -> RenderTree -> Html Msg
renderTree e t =
  let
    render = renderTree e
  in
    case t of
      Node n children ->
        if n.render then
          div [ class "tree-div" ] ( map render children ++ [ renderTerm e n.term ] )
        else
          div [] []


renderTerm : Env -> Term -> Html Msg
renderTerm e t =
  let
    spanClass =
      case typecheck e t of
        Checks _    -> "type-checks"
        Fails _ _ _ _ -> "type-fails"
        Partial _   -> "type-partial"
        Invalid     -> "type-fails"
    checkResult = typecheck e t
  in
    div [ class "text-div" ]
    [ renderTermInline checkResult t
    , text " : "
    , span [ class spanClass ] [ text (checkResultToString checkResult) ]
    ]


listSubterms : Term -> List Term
listSubterms t =
  case t of
    CTerm _ ->   []
    VTerm _ ->   []
    Plus x y ->  [x, y]
    Minus x y -> [x, y]
    Times x y -> [x, y]
    Eq x y ->    [x, y]
    And x y ->   [x, y]
    Or x y ->    [x, y]
    Lam x y ->   [x, y]
    App x y ->   [x, y]
    Tuple x y -> [x, y]
    _ ->         []

renderSubtermsRec : Int -> List Term -> CheckResult -> List (Html Msg)
renderSubtermsRec i ts c =
  let
    renderNext = renderSubtermsRec (i + 1)
    isFail =
      case c of
        Fails argNum exp got out ->
          i == argNum
        _ ->
          False
  in
    case ts of
      [] -> [ text "" ]
      [t] ->
        case isFail of
          True  -> [ span [class "error-subterm"] [text (Evaluate.termToString t), renderErrorDiv c] ]
          False -> [ text (Evaluate.termToString t) ]
      t :: ts2 ->
        case isFail of
          True -> [ span [class "error-subterm"] [text (Evaluate.termToString t), renderErrorDiv c] ]  ++
            renderNext ts2 c
          False -> [ text (Evaluate.termToString t) ] ++
            renderNext ts2 c

intersperse : a -> List a -> List a
intersperse i xs =
  case xs of
    [] -> []
    [x] -> [i, x]
    x :: rem -> [i, x] ++ intersperse i rem

renderSubterms : List Term -> CheckResult -> List (Html Msg)
renderSubterms t c =
  let
    subterms = intersperse ( text " " ) ( renderSubtermsRec 1 t c )
  in
    case subterms of
      x :: xs -> xs
      _       -> []


-- renders the inline portion of the term, which may contain spans
renderTermInline : CheckResult -> Term -> Html Msg
renderTermInline result t =
  let
    argTerms = listSubterms t
    isOp =
      case t of
        CTerm _   -> False
        VTerm _   -> False
        EmptyTree -> False
        _         -> True
    opStr =
      case t of
        CTerm _   -> ""
        VTerm _   -> ""
        Plus _ _  -> "+"
        Minus _ _ -> "-"
        Times _ _ -> "*"
        Eq _ _    -> "=="
        And _ _   -> "&&"
        Or _ _    -> "||"
        Lam _ _   -> "->"
        App _ _   -> " "
        Tuple _ _ -> ","
        _         -> ""

    subterms = renderSubterms argTerms result
  in
    case isOp of
      True ->
        case subterms of
          x :: xs ->
            case t of 
              Tuple _ _ -> span [] ([text ("(")] ++ [x, text (" " ++ opStr)] ++ xs ++ [text (")")]) 
              _         -> span [] ([x, text (" " ++ opStr)] ++ xs)
            
          _ ->
            text "rendering error"
      False ->
        text (Evaluate.termToString t)


renderErrorDiv : CheckResult -> Html Msg
renderErrorDiv c =
  case c of
    Fails _ exp got out ->
      let
        expStr = typeToString exp
        gotStr = typeToString got
      in
        div [class "error-details"] [ text ("Expected: " ++ expStr), br [] [], text ("Got: " ++ gotStr) ]

    _ -> div [class "error-details"] []

-- Recursive function that finds the correct tree in the old list of trees and changes it by creating a new list of render trees.
{-newRenderTree : List RenderTree -> List RenderTree -> RenderTree -> Int -> Env -> List RenderTree
newRenderTree olrdRTS newRTS (Node nTerm _) depth env = case olrdRTS of
  []      -> newRTS
  (r::rs) -> if r.term == tree.term
              then let newRT = genRenderTree (r.renderDepth + depth) env r.term
                    in newRenderTree rs (newRTS ++ [newRT]) tree depth env
              else newRenderTree rs (newRTS ++ [r]) tree depth env-}


type alias RenderTree = Tree RenderNode

type alias RenderNode =
  { render: Bool
  , term: Term }

{-
RenderTreeInfo contains all of the information necessary to generate a
RenderTree
-}
type alias RenderTreeInfo =
  { id: Int
  , var: Var
  , depth: Int }


-- creates a render tree from a rtInfo and an environment
genRenderTree2 : RenderTreeInfo -> Env -> RenderTree
genRenderTree2 rtInfo env =
  genRenderTree rtInfo.depth env rtInfo.var.term

genRenderTree : Int -> Env -> Term -> RenderTree
genRenderTree depth e t =
  let
    dnew = depth - 1
    gTree = genRenderTree dnew e
    checkStatus = typecheck e t
    children =
      case t of
        CTerm _   -> []
        VTerm x   ->
          case lookup e x of
            Just subst -> [gTree subst]
            Nothing    -> []
        Plus x y  -> [gTree x, gTree y]
        Minus x y -> [gTree x, gTree y]
        Times x y -> [gTree x, gTree y]
        Eq x y    -> [gTree x, gTree y]
        And x y   -> [gTree x, gTree y]
        Or x y    -> [gTree x, gTree y]
        -- Lam x y   -> [gTree x, gTree y]
        App x y   -> [gTree x, gTree y]
        Tuple x y -> [gTree x, gTree y]
        _         -> []

    n =
      -- always render render nodes that don't pass typecheck
      case checkStatus of
        Checks _ ->
          { render = (depth > 0)
          , term = t}

        _ ->
          { render = True
          , term = t}

  in
    Node n children


printPT : List Var -> List (Html Msg)
printPT vars =
  case vars of
    []-> [div [class "tkns-div"] [text ""]]
    (l::ls) -> [div [class "tkns-div"] [text (toString l.term)]] ++ (printPT ls)


printRT : List Var -> List RenderTreeInfo -> List (Html Msg)
printRT vars rtInfos =
  case rtInfos of
    [] -> [div [class "tkns-div"] [text ""]]
    (ri::rs) ->
      let
        rt = genRenderTree2 ri (Environment.varsToEnv vars)
      in
        [div [ class "flex-container" ]
                      [
                              div [class "tree-container"] [ renderTree (Environment.varsToEnv vars) rt ]
                          ,   div [ class "ui-div" ]
                              [
                                renderSummary (Environment.varsToEnv vars) rt
                                , h3 [class "css-title"] [text "Depth:"]
                                , div [class "button-container"]
                                  [ div [ class "buttons" ]
                                    [ button [ onClick (DecDepth ri.id) ] [ text "-" ]
                                    , text ( String.fromInt ri.depth )
                                    , button [ onClick (IncDepth ri.id) ] [ text "+" ]
                                    ]
                                  ]
                              ]
                      ]] ++ (printRT vars rs)
