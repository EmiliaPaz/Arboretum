port module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, input, span, textarea, br, p)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List exposing (map, filterMap, head, tail, reverse, filter)
import Debug exposing (toString)
import Json.Decode as Decode exposing (Decoder, field, bool, int, string)
import String exposing (split)
import Dict exposing (Dict)

import Evaluate exposing (Val)
import Render exposing (renderCallTree)
import Stack
import Tree exposing (Tree)
import Types exposing (..)
import Typecheck exposing (TSubst, CallTree, typecheck, typecheckAll, typeToString, tsubstToString)


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
  , terms : TermEnv
  , annotations : TypeEnv
  , renderInfos : List RenderInfo
  , errorMsg : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { content = ""
    , terms = Dict.empty
    , annotations = Dict.empty
    , renderInfos = []
    , errorMsg = ""
    }
  , Cmd.none )


-- UPDATE

type Msg
  = Change String | IncDepth String | DecDepth String | GotAsts (Result String (List (String, Either Term VType)))



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      let
        lines = split "\n" newContent
      in
        ({ model | content = newContent }, parseLines lines)

    IncDepth name ->
      let
        infos =
            filterUpdate (\i -> i.name == name) (\i -> {i | depth = i.depth + 1}) model.renderInfos
      in
        ({model | renderInfos = infos} , Cmd.none )

    DecDepth name ->
      let
        infos =
            filterUpdate (\i -> i.name == name) (\i -> {i | depth = i.depth - 1}) model.renderInfos
      in
        ({model | renderInfos = infos} , Cmd.none )

    GotAsts r ->
      case r of
        Ok ts ->
          let
            renderInfos = buildAllRenderInfos (filterTerms ts) model.renderInfos
          in
            ({ model | renderInfos = renderInfos }
            , Cmd.none)

        Err e ->
          ({ model | errorMsg = e }, Cmd.none)


filterTerms : List (String, Either a b) -> List (String, a)
filterTerms xs =
  filterMap (\(n,x) ->
    case x of
      Left l -> Just (n, l)
      Right _ -> Nothing
  ) xs


filterAnnotations : List (String, Either a b) -> List (String, b)
filterAnnotations xs =
  filterMap (\(n,x) ->
    case x of
      Right l -> Just (n, l)
      Left _ -> Nothing
  ) xs


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


-- PORTS
port parseLines : List String -> Cmd a
port gotAst : (Decode.Value -> msg) -> Sub msg


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  gotAst (decodeAsts >> GotAsts)


type Either a b = Left a | Right b

-- an ast either will produce a term or a type
decodeAsts : Decode.Value -> Result String (List (String, Either Term VType))
decodeAsts v =
  case Decode.decodeValue (Decode.list stmtDecoder) v of
    Ok t -> Ok t
    Err e -> Err (Decode.errorToString e)

stmtDecoder : Decoder (String, Either Term VType)
stmtDecoder =
  field "type" string
    |> Decode.andThen
      (\t ->
        case t of
          "TYPE_STMT" ->
            typeAssignDecoder
              |> Decode.andThen (\(n, ty) -> Decode.succeed (n, Right ty))

          "ASSIGN_STMT" ->
            assignDecoder
              |> Decode.andThen (\(n, tm) -> Decode.succeed (n, Left tm))

          _ ->
            Decode.fail ("Unrecognized statement type: " ++ t)
      )

typeAssignDecoder : Decoder (String, VType)
typeAssignDecoder =
  Decode.map2 (\n t -> (n, t))
    (field "identifier" string)
    (field "assignType" typeDecoder)

typeDecoder : Decoder VType
typeDecoder =
  field "type" string
    |> Decode.andThen
      (\t ->
        case t of
          "FN_TYPE"    -> fnTypeDecoder
          "BASIC_TYPE" -> basicTypeDecoder
          _            -> Decode.fail ("Unrecognized type: " ++ t)
      )

fnTypeDecoder : Decoder VType
fnTypeDecoder =
  field "children" (Decode.list typeDecoder)
    |> Decode.andThen
      (\ts ->
        case ts of
          t1::t2::tr ->
            Decode.succeed (binCombinerRight (\l r -> TFun l r) t1 ([t2] ++ tr))
          _ ->
            Decode.fail "function type has fewer than 2 children"
      )

basicTypeDecoder : Decoder VType
basicTypeDecoder =
  field "value" string
    |> Decode.andThen
      (\t ->
        case t of
          "Int"  -> Decode.succeed TInt
          "Bool" -> Decode.succeed TBool
          _      -> Decode.fail ("Invalid basic type: " ++ t)
      )

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
    "DIV_EXPR"  -> divDecoder
    "MOD_EXPR" -> modDecoder
    "AND_EXPR" -> andDecoder
    "OR_EXPR" -> orDecoder
    "EQ_EXPR" -> eqDecoder
    "FN_EXPR" -> fnDecoder
    "IF_EXPR" -> ifDecoder
    "APP_EXPR" -> appDecoder
    "TUPLE" -> tupleDecoder
    _      -> Decode.fail ("unrecognized type: " ++ s)

fnDecoder : Decoder Term
fnDecoder =
  Decode.map2 (\n t -> Lam n t)
    (field "variable" string)
    (field "body" exprDecoder)

ifDecoder : Decoder Term
ifDecoder =
  Decode.map3 (\c f s -> If c f s)
    (field "condition" exprDecoder)
    (field "first" exprDecoder)
    (field "second" exprDecoder)

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
binCombiner : (a -> a -> a) -> a -> List a -> a
binCombiner comb first ts =
  case ts of
    [] ->
      first
    t::tr ->
      comb (binCombiner comb t tr) first

-- Right-associative version of binCombinder
binCombinerRight : (a -> a -> a) -> a -> List a -> a
binCombinerRight comb first ts =
  case ts of
    [] ->
      first
    t::tr ->
      comb first (binCombinerRight comb t tr)

addDecoder = binDecoder (\t1 t2 -> BinTerm Plus t1 t2)
subtDecoder = binDecoder (\t1 t2 -> BinTerm Minus t1 t2)
multDecoder = binDecoder (\t1 t2 -> BinTerm Times t1 t2)
divDecoder = binDecoder (\t1 t2 -> BinTerm Div t1 t2)
modDecoder = binDecoder (\t1 t2 -> BinTerm Mod t1 t2)
andDecoder = binDecoder (\t1 t2 -> BinTerm And t1 t2)
orDecoder = binDecoder (\t1 t2 -> BinTerm Or t1 t2)
appDecoder = binDecoder (\t1 t2 -> App t1 t2)

eqDecoder : Decoder Term
eqDecoder =
  Decode.map2 (\l r -> (l, r))
    (field "lhs" exprDecoder)
    (field "rhs" exprDecoder)
    |> Decode.andThen
      (\(l, r) -> Decode.succeed (BinTerm Eq l r))

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

tupleDecoder : Decoder Term
tupleDecoder =
  Decode.map2 (\l r -> Tuple l r )
    (field "left" exprDecoder)
    (field "right" exprDecoder)

-- VIEW

view : Model -> Document Msg
view model =
  { title = "TreeScript"
  , body =
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
    , p [] [text model.errorMsg]
    , div [class "tokenizer-parser-title-container"]
      [ div [class "textarea-container"]
        [ h3 [ class "css-title" ] [text "Input Program:"]
        , textarea [ rows 10, cols 50, placeholder "Text to render", value model.content, onInput Change ] []
        ]
      ]
      , div [ class "flexs-container" ]
        [ div [class "tree-title-container"]
          [ h3 [class "css-title"] [text "Derivation Tree:"]
          , div [class "trees-container"] (map renderWithUI model.renderInfos)
          ]
        ]
    ]
  }


renderSummary : RenderInfo -> Html Msg
renderSummary info =
  let
    typeString = 
      case Typecheck.finalType info.typecheck of
        Just t -> Typecheck.typeToString t
        Nothing -> "Error"
    
    substString = 
      case info.typecheck of
        Tree.Tree t -> case t.node.subs of
          Just s -> Typecheck.tsubstToString s
          Nothing -> "Error"
  in
    div [ class "summary" ]
      [ h1 [ class "summary-title" ] [ text "Summary:" ]
      , text ( "Evaluation result: " ++ Evaluate.valToString info.evaluation )
      , br [] []
      , text ( "Typecheck: " ++ typeString )
      , br [] []
      , text ( "Substitutions: " ++ substString)
      ]


type alias RenderInfo =
  { name: String
  , term: Term
  , evaluation: Maybe Val
  , typecheck: CallTree
  , depth: Int }


buildAllRenderInfos : List (String, Term) -> List RenderInfo -> List RenderInfo
buildAllRenderInfos terms infos =
  let
    termEnv = List.foldl (\(name, term) env -> Dict.insert name term env) Dict.empty terms

    vals = List.map (\(name, term) -> Evaluate.eval termEnv term) terms
    checks = Typecheck.typecheckAll terms

    findInfo name is =
      is
        |> List.filter (\i -> i.name == name)
        |> List.head
  in
    List.map3 (\(name, term) val check -> 
      case findInfo name infos of
        Just info ->
          { info
            | term = term
            , evaluation = val
            , typecheck = check 
          }
        
        Nothing ->
          { name = name
          , term = term
          , evaluation = val
          , typecheck = check
          , depth = 3
          }
          ) terms vals checks


renderWithUI : RenderInfo -> Html Msg
renderWithUI info =
  div [ class "flex-container" ]
    [ div [class "tree-container"] [ renderCallTree info.typecheck info.depth ]
    , div [ class "ui-div" ]
      [ renderSummary info
      , h3 [class "css-title"] [text "Depth:"]
      , div [class "button-container"]
        [ div [ class "buttons" ]
          [ button [ onClick (DecDepth info.name) ] [ text "-" ]
          , text ( String.fromInt info.depth )
          , button [ onClick (IncDepth info.name) ] [ text "+" ]
          ]
        ]
      ]
    ]
