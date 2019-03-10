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
  , renderTrees : List RenderTree
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { content = "", tokens = [[]], vars = [], renderTrees = [{ render=False, renderDepth=0, term=EmptyTree, children= RenderChildren []}] }, Cmd.none )



-- UPDATE

type Msg
  = Change String | IncDepth RenderTree Env | DecDepth RenderTree Env

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      let
        c = newContent
        t = Tokenizer.tokenize (map (String.words) (String.lines c))
        v = map Parser.parse t
        rs = map (\vs -> genRenderTree 3 (lookup v) vs.term ) v
      in
      ({ model |
          content = c
        , tokens = t
        , vars = v
        , renderTrees = rs
       }, Cmd.none)
    IncDepth r env -> let rs = findRenderTree model.renderTrees r 1 env in (model, Cmd.none ) 
    DecDepth r env-> let rs = findRenderTree model.renderTrees r -1 env in (model , Cmd.none ) 

findRenderTree : List RenderTree -> RenderTree -> Int -> Env -> RenderTree
findRenderTree renderTrees tree depth env = case renderTrees of
  []      -> tree -- should never get here
  (r::rs) -> if r.term == tree.term 
              then let newR = genRenderTree (r.renderDepth + 1) env r.term
                    in { r | render = newR.render, renderDepth = newR.renderDepth, term = newR.term, children = newR.children } 
              else findRenderTree rs tree depth env
              



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
            -- , div [ class "expression-builder" ] [ text (Parser.printParseTrees model.vars) ]
            , div [class "expression-builder"] (printPT model.vars)
          ]
        ]
      , div [ class "flex-container" ]
      [
         div [class "tree-title-container"]
         [
           h3 [class "css-title"] [text "Derivation Tree:"]
           -- , div [ class "tree-container" ] [ div [] [ renderTree (lookup model.vars) model.renderTree ] ]
             , div [class "tree-container"] (printRT model.vars model.renderTrees)
         ]
        -- , div [ class "ui-div" ]
        -- [
        --   renderSummary model
        --   , h3 [class "css-title"] [text "Depth:"]
        --   , div [ class "buttons" ]
        --     [ button [ onClick DecDepth ] [ text "-" ]
        --     , text ( String.fromInt model.renderTree.renderDepth )
        --     , button [ onClick IncDepth ] [ text "+" ] ]
        -- ]
      ]
    ]
 }

printTknsLBL : List (List Token) -> List (Html Msg)
printTknsLBL tkns =
  case tkns of
    []-> [div [class "tkns-div"] [text ""]]
    (l::ls) -> [div [class "tkns-div"] [text (Tokenizer.tokenizePrint l)]] ++ (printTknsLBL ls)

renderSummary : Env -> RenderTree -> Html Msg
renderSummary envr r =
  div [ class "summary" ]
  [ h1 [ class "summary-title" ] [ text "Summary:" ]
  , text ( "Evaluation result: " ++ Render.valToString (Render.eval envr r.term ) )
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
        EmptyTree -> RenderChildren []

  in
  { render = True
  , renderDepth = depth
  , term = t
  , children = c}



printPT : List Var -> List (Html Msg)
printPT vars =
  case vars of
    []-> [div [class "tkns-div"] [text ""]]
    (l::ls) -> [div [class "tkns-div"] [text (toString l.term)]] ++ (printPT ls)


printRT : List Var -> List RenderTree -> List (Html Msg)
printRT vars renderTrees =
  case renderTrees of
    []-> [div [class "tkns-div"] [text ""]]
    (r::rs) -> [div [ class "flex-container" ]
                    [
                            div [class "tree-container"] [ renderTree (lookup vars) r ]
                        ,   div [ class "ui-div" ]
                            [
                              renderSummary (lookup vars) r
                              , h3 [class "css-title"] [text "Depth:"]
                              , div [ class "buttons" ]
                                [ button [ onClick (DecDepth r (lookup vars)) ] [ text "-" ]
                                , text ( String.fromInt r.renderDepth )
                                , button [ onClick (IncDepth r (lookup vars)) ] [ text "+" ] ]
                            ]
                    ]] ++ (printRT vars rs)
