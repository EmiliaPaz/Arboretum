import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, input, span, textarea)
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
  , parseTree  : Term
  , env        : Env
  , renderTree : RenderTree
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    testVars = [{ name = "a", term = CTerm (CInt 5)}]
    testInput = "( ( ( ( 5 + 1 ) * 7 ) == ( 21 * a ) ) && True )"
    testTokens = Tokenizer.tokenize (map (String.words) (String.lines testInput))
    testTerm = Parser.parse([TokConstInt 5])
    testEnv = lookup testVars
    testDepth = 3
    testRender = genRenderTree testDepth testEnv testTerm
  in
  ( { content = "", tokens = testTokens, parseTree = testTerm, env = testEnv, renderTree = testRender }, Cmd.none )

-- test cases:
--testVars = [{ name = "a", term = CTerm (CInt 5)}]
--testTerm = And (Eq (Times (Plus (CTerm (CInt 5)) (CTerm (CInt 1))) (CTerm (CInt 7))) (Times (CTerm (CInt 21)) (VTerm "a"))) (CTerm (CBool True))
failTerm = Or (Eq (CTerm (CInt 1)) (CTerm (CBool True))) (CTerm (CBool False))
--testDepth = 3

-- UPDATE

type Msg
  = Change String | IncDepth | DecDepth


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      let
        c = newContent
        t = Tokenizer.tokenize (map (String.words) (String.lines c))
        p = Parser.parse [TokConstInt 5]
        r = genRenderTree model.renderTree.renderDepth model.env p
      in
      ({ model |
          content = c
        , tokens = t
        , parseTree = p
        , renderTree = r
       }, Cmd.none)
    IncDepth -> ({ model | renderTree = genRenderTree (model.renderTree.renderDepth + 1) model.env model.parseTree }, Cmd.none)
    DecDepth -> ({ model | renderTree = genRenderTree (model.renderTree.renderDepth - 1) model.env model.parseTree }, Cmd.none)


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
        [ textarea [ rows 15, cols 50, placeholder "Text to render", value model.content, onInput Change ] []
        , h3 [ class "css-title" ] [text "Tokens:"]
        , div [ class "expression-builder" ] [ text (Tokenizer.printTokens(model.tokens))]
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
          , text ( String.fromInt model.renderTree.renderDepth )
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
  if t.render && t.renderDepth > 0 then
    div [ class "tree-div" ] ( renderChildren e t.children ++ [ renderTerm e t.term ] )
  else
    div [] []


renderChildren : Env -> RenderChildren -> List (Html Msg)
renderChildren e (RenderChildren c) =
  map (renderTree e) c


renderTerm : Env -> Term -> Html Msg
renderTerm e t =
  div [ class "text-div" ]
  [ text (Render.termToString t ++ " : ")
  , span [ class "type-span" ] [ text (Render.typeToString (Render.typecheck e t)) ]
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

type alias RenderTree =
  { render: Bool
  , renderDepth: Int
  , term: Term
  , children: RenderChildren}

type RenderChildren = RenderChildren (List RenderTree)

genRenderTree : Int -> Env -> Term -> RenderTree
genRenderTree depth e t =
  let
    dnew = depth - 1
    gTree = genRenderTree dnew e
    c =
      case t of
        CTerm _   -> RenderChildren []
        VTerm x   ->
          case e x of
            Just subst -> RenderChildren [gTree subst]
            Nothing    -> RenderChildren []
        Plus x y  -> RenderChildren [gTree x, gTree y]
        Minus x y -> RenderChildren [gTree x, gTree y]
        Times x y -> RenderChildren [gTree x, gTree y]
        Eq x y    -> RenderChildren [gTree x, gTree y]
        And x y   -> RenderChildren [gTree x, gTree y]
        Or x y    -> RenderChildren [gTree x, gTree y]
        EmptyTree -> RenderChildren [gTree EmptyTree, gTree EmptyTree] --- not valid, just for debuging

  in
  { render = True
  , renderDepth = depth
  , term = t
  , children = c}
