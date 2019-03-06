import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, input, span, br)
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
  { content     : String
  , tokens      : List Token
  , parseTree   : Term
  , env         : Env
  , renderTree  : RenderTree
  , renderDepth : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    testVars = [{ name = "a", term = CTerm (CInt 5)}]
    testInput = "( ( ( ( 5 + 1 ) * 7 ) == ( 21 * a ) ) && True )"
    testTokens = Tokenizer.tokenize (String.words testInput)
    testTerm = Parser.parse(testTokens)
    testEnv = lookup testVars
    testDepth = 3
    testRender = genRenderTree testDepth testEnv testTerm
  in
  ( { content = "", tokens = testTokens, parseTree = testTerm, env = testEnv, renderTree = testRender, renderDepth = testDepth}, Cmd.none )

-- test cases:
--testVars = [{ name = "a", term = CTerm (CInt 5)}]
--testTerm = And (Eq (Times (Plus (CTerm (CInt 5)) (CTerm (CInt 1))) (CTerm (CInt 7))) (Times (CTerm (CInt 21)) (VTerm "a"))) (CTerm (CBool True))
failTerm = Or (Eq (CTerm (CInt 1)) (CTerm (CBool True))) (CTerm (CBool False))

-- UPDATE

type Msg
  = Change String | IncDepth | DecDepth


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model |
          content = newContent
        , tokens = Tokenizer.tokenize (String.words newContent)
        , parseTree = Parser.parse (Tokenizer.tokenize (String.words newContent))
        , renderTree = genRenderTree model.renderDepth model.env (Parser.parse (Tokenizer.tokenize (String.words newContent)))
       }, Cmd.none)
    IncDepth ->
      ({ model | 
          renderDepth = model.renderDepth + 1
        , renderTree = genRenderTree (model.renderDepth + 1) model.env model.parseTree 
       }, Cmd.none)
    DecDepth ->
      ({ model | 
          renderDepth = model.renderDepth - 1
        , renderTree = genRenderTree (model.renderDepth - 1) model.env model.parseTree 
       }, Cmd.none)


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
      div []
        [ input [ placeholder "Text to render", value model.content, onInput Change ] []
        , h3 [ class "css-title" ] [text "Tokens:"]
        , div [ class "expression-builder" ] [ text (Tokenizer.tokenizePrint(model.tokens))]
        , h3 [ class "css-title" ] [text "Parse Tree:"]
        , div [ class "expression-builder" ] [ text (toString(model.parseTree)) ]
        ]
    , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
    , div [ class "flex-container" ]
      [ h3 [class "css-title"] [text "Rendered Tree:"]
      ,  div [ class "tree-container" ] [ div [] [ renderTree model.env model.renderTree ] ]
      , div [ class "ui-div" ]
        [ renderSummary model
        , h3 [class "css-title"] [text "Depth:"]
        , div [ class "buttons" ]
          [ button [ onClick DecDepth ] [ text "-" ]
          , text ( String.fromInt model.renderDepth )
          , button [ onClick IncDepth ] [ text "+" ] ]
        ]
      ]
    ]
 }

renderSummary : Model -> Html Msg
renderSummary model =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ Render.valToString (Render.eval model.env model.parseTree) )
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
      case Render.typecheck3 e t of
        Checks _    -> "type-checks"
        Fails _ _ _ -> "type-fails"
        Partial _   -> "type-partial"
        Invalid     -> "type-fails"
  in
  div [ class "text-div" ]
  [ text (Render.termToString t ++ " : ")
  , span [ class spanClass ] [ text (Render.checkResultToString (Render.typecheck3 e t)) ]
  ]


-------------------- NEW
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

genRenderTree : Int -> Env -> Term -> RenderTree
genRenderTree depth e t =
  let
    dnew = depth - 1
    gTree = genRenderTree dnew e
    c =
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
        EmptyTree -> []

    n = 
      { render = (depth > 0)
      , term = t}

  in
    Node n c