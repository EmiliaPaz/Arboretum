import Browser
import Html exposing (Html, button, div, text, h1)
import Html.Events exposing (onClick)
import String exposing (concat)
import List exposing (head,tail)
import Debug exposing (toString)

main = Browser.sandbox {init = init, update = update, view = view}

-- MODEL

type alias Model = Int

init : Model
init =
  0

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-------- Tree implementation: in process --------
type Operator = Plus | Minus | Times

type Token = TokOp Operator | TokNum Int

tokenEx = [TokNum 3,TokOp Plus,TokNum 5]

-- Parser

type Tree = SumNode Operator Tree Tree 
            | ProdNode Operator Tree Tree
            | NumNode Int
            | UnaryNode Operator Tree
            | EmptyTree

toMaybe : List Token -> Maybe (List Token)
toMaybe tokens = case tokens of
                  [] -> Nothing
                  _ -> Just tokens

fromMaybeList : Maybe (List Token) -> List Token
fromMaybeList ls = case ls of
                  Nothing -> []
                  Just list -> list
                  

parse : List Token -> Tree
parse tokens = let (tree, toks) = expression tokens
                in tree

expression : List Token -> (Tree, List Token)
expression tokens = let (termTree, tokens2) = term tokens
                      in case head tokens2 of
                        Just (TokOp op) -> let (expTree, tokens3) = expression (fromMaybeList(tail tokens2))
                                            in (SumNode op termTree expTree, tokens3)
                        Just (TokNum n) -> (termTree, tokens2)
                        Nothing -> (termTree, tokens2)

term : List Token -> (Tree, List Token)
term tokens =
    case head tokens of
        Just (TokNum p)  -> (NumNode p, fromMaybeList(tail tokens))
        Just (TokOp op)  -> (EmptyTree, [])
        Nothing          -> (EmptyTree, [])


-- Helper functions
oneAhead : List Token -> Maybe Token
oneAhead list = 
    case list of 
        [] -> Nothing
        t::ts -> Just t



----------- end of tree implementation -----------


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (toString (parse [TokNum 3, TokOp Plus, TokNum 5])) ]
    ]
