import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, input, span)
import Html.Events exposing (onInput)
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
  { content : String,
    tokens  : List Token, 
    parseTree   : Term
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( {content = "", tokens = [], parseTree = EmptyTree}, Cmd.none )
  

-- UPDATE

type Msg
  = Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | content = newContent , tokens = Tokenizer.tokenize (String.words newContent) , parseTree = Parser.parse (Tokenizer.tokenize (String.words newContent))   }, Cmd.none)



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
        [ input [ placeholder "Text to tokenize", value model.content, onInput Change ] []
        , div [ class "expression-builder" ] [ text (Tokenizer.tokenizePrint(model.tokens)) ]
        , div [ class "expression-builder" ] [ text (toString(model.parseTree)) ]
        ]
    , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "trees.css" ] []
    , div [ class "tree-container" ] [ div [] [ renderTree (model.parseTree) ] ]
    , renderSummary model
    ]
 }

renderSummary : Model -> Html Msg
renderSummary model =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ Render.valToString (Render.eval model.parseTree) )
  ]

renderTree : Term -> Html Msg
renderTree t =
  case t of
    CTerm x ->
      div [ class "tree-div" ] [ renderTerm t ]
    
    VTerm x ->
      div [ class "tree-div" ]
      [ renderTree x.term
      , renderTerm t ]

    Plus x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Minus x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Times x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Eq x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 
    
    And x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 

    Or x y ->
      div [class "tree-div"]
      [ renderTree x
      , renderTree y
      , renderTerm t 
      ] 
    
    EmptyTree -> div [class "tree-div"]
      [ div [ class "text-div" ] [ text ( "") ] ] 


renderTerm : Term -> Html Msg
renderTerm t =
  div [ class "text-div" ]
  [ text (Render.termToString t ++ " : ")
  , span [ class "type-span" ] [ text (Render.typeToString (Render.typecheck t)) ]
  ]