import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, input, span, textarea, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List exposing (map,head,tail)
import Debug exposing (toString)

import Tokenizer
import Parser
import Render
import Types exposing (..)


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
  , tokens  : List (List Token)
  , vars       : List Var
  , renderTreeInfos : List RenderTreeInfo
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { content = ""
    , tokens = [[]]
    , vars = []
    , renderTreeInfos = []
    }
  , Cmd.none )



-- UPDATE

type Msg
  = Change String | IncDepth Int | DecDepth Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      let
        c = newContent
        t = Tokenizer.tokenize (map (String.words) (String.lines c))
        v = map Parser.parse t
        rs = genRenderInfos 3 v
      in
      ({ model |
          content = c
        , tokens = t
        , vars = v
        , renderTreeInfos = rs
       }, Cmd.none)

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


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Document Msg
view model =
 { title = "Tree Assembly"
  , body =
    [
      Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
      , div [class "tokenizer-parser-title-container"]
        [
          div [class "textarea-container"]
          [
            h3 [ class "css-title" ] [text "Input Program:"]
            , textarea [ rows 10, cols 50, placeholder "Text to render", value model.content, onInput Change ] []
          ]
          , div [class "tokenizer-parser-container"]
          [
            h3 [ class "css-title" ] [text "Tokens:"]
            , div [class "expression-builder"] (printTknsLBL model.tokens)
            , h3 [ class "css-title" ] [text "Parse Tree:"]
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
  , text ( "Evaluation result: " ++ Render.valToString (Render.eval envr rNode.term) )
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
      case Render.typecheck e t of
        Checks _    -> "type-checks"
        Fails _ _ _ _ -> "type-fails"
        Partial _   -> "type-partial"
        Invalid     -> "type-fails"
    checkResult = Render.typecheck e t
  in
    div [ class "text-div" ]
    [ renderTermInline checkResult t 
    , text " : "
    , span [ class spanClass ] [ text (Render.checkResultToString checkResult) ]
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
          True  -> [ span [class "error-subterm"] [text (Render.termToString t), renderErrorDiv c] ]
          False -> [ text (Render.termToString t) ]
      t :: ts2 ->
        case isFail of
          True -> [ span [class "error-subterm"] [text (Render.termToString t), renderErrorDiv c] ]  ++
            renderNext ts2 c
          False -> [ text (Render.termToString t) ] ++
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
        _         -> ""

    subterms = renderSubterms argTerms result
  in
    case isOp of
      True -> 
        case subterms of
          x :: xs ->
            span [] ([x, text (" " ++ opStr)] ++ xs)
          _ ->
            text "rendering error"
      False ->
        text (Render.termToString t)


renderErrorDiv : CheckResult -> Html Msg
renderErrorDiv c =
  case c of
    Fails _ exp got out ->
      let
        expStr = Render.typeToString exp
        gotStr = Render.typeToString got
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


{-
attempts to find a Term for s in e; currying lookup with a list of vars
produces an Env
-}

lookup : List Var -> String -> Maybe Term
lookup e s =
  case e of
    [] ->
      Nothing

    v :: vs ->
      if v.name == s then
        Just v.term
      else lookup vs s

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
    checkStatus = Render.typecheck e t
    children =
      case t of
        CTerm _   -> []
        VTerm x   ->
          case e x of
            Just subst -> [gTree subst]
            Nothing    -> []
        Plus x y  -> [gTree x, gTree y]
        Minus x y -> [gTree x, gTree y]
        Times x y -> [gTree x, gTree y]
        Eq x y    -> [gTree x, gTree y]
        And x y   -> [gTree x, gTree y]
        Or x y    -> [gTree x, gTree y]
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
        rt = genRenderTree2 ri (lookup vars)
      in
        [div [ class "flex-container" ]
                      [
                              div [class "tree-container"] [ renderTree (lookup vars) rt ]
                          ,   div [ class "ui-div" ]
                              [
                                renderSummary (lookup vars) rt
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
