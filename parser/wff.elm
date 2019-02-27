import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, input, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import String exposing (concat)
import List exposing (map,head,tail)
import Debug exposing (toString)

main = Browser.sandbox {init = init, update = update, view = view}

-- MODEL

type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }

-- UPDATE

type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }


-------------------------------------- Tokenizer --------------------------------------
type Token = TokPlus | TokMinus | TokTimes | TokAssign | TokEq | TokAnd | TokOr | TokLParen | TokRParen | TokVar String | TokConstInt String | TokConstBool String | TokInvalid | TokEnd

operator : String -> Token
operator str =
            case str of
                "" -> TokInvalid
                other ->
                    if (str == "+")
                        then TokPlus
                     else if (str == "-")
                         then TokMinus
                     else if (str == "*")
                         then TokTimes
                     else if (str == "=")
                         then TokAssign
                     else if (str == "==")
                         then TokEq
                     else if (str == "&&")
                         then TokAnd
                     else if (str == "||")
                         then TokOr
                     else TokInvalid

operators = ["+", "-", "*", "=", "==", "&&", "||"]

isBoolean : String -> Bool
isBoolean x =
    case x of
        "True" -> True
        "False" -> True
        other -> False


tokenize : List String -> List Token
tokenize str =
        case str of
            [] -> []
            (x::xs) ->
                if (List.member x operators)
                    then (operator x) :: tokenize xs
                else if (isBoolean x)
                    then TokConstBool x :: tokenize xs
                else if (String.filter Char.isDigit x == x && x /= "")
                    then TokConstInt x :: tokenize xs
                else if (String.filter Char.isLower x == x && x /= "")
                    then TokVar x :: tokenize xs
                else if (x == "(")
                    then TokLParen :: tokenize xs
                else if (x == ")")
                    then TokRParen :: tokenize xs
                else if (x == "")
                    then tokenize xs
                else TokInvalid :: tokenize xs

tokenizePrint : List Token -> String
tokenizePrint tokens =
    case tokens of
        [] -> ""
        (x::xs) ->
            case x of
                _ -> (Debug.toString x) ++ ", " ++ tokenizePrint xs



type alias Var = 
  { name: String
  , term: Term }


-------------------------------------- Parser --------------------------------------
type Term = CTerm Const | VTerm Var | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term | EmptyTree
type Const = CBool Bool | CInt String


toMaybe : List Token -> Maybe (List Token)
toMaybe tokens = case tokens of
                  [] -> Nothing
                  _ -> Just tokens

fromMaybeList : Maybe (List Token) -> List Token
fromMaybeList ls = case ls of
                  Nothing -> []
                  Just list -> list

parse : List Token -> Term
parse tokens = let (tree, toks) = expression tokens
                in tree

expression : List Token -> (Term, List Token)
expression tokens = let (termTree, tokens2) = term tokens
                      in case head tokens2 of
                        Just (TokPlus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Plus termTree expTree, tokens3)
                        Just (TokMinus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Minus termTree expTree, tokens3)
                        Just (TokTimes) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Times termTree expTree, tokens3)
                        Just (TokConstInt n) -> (termTree, tokens2)
                        _ -> (termTree, tokens2)

term : List Token -> (Term, List Token)
term tokens =
    case head tokens of
        Just (TokConstInt p)  -> (CTerm (CInt p), fromMaybeList(tail tokens))
        Just (TokPlus)  -> (EmptyTree, [])
        Just (TokMinus)  -> (EmptyTree, [])
        Just (TokTimes)  -> (EmptyTree, [])
        _          -> (EmptyTree, [])


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
    [ input [ placeholder "Text to tokenize", value model.content, onInput Change ] []
    , div [] [ text (tokenizePrint(tokenize(String.words model.content))) ]
    , div [] [ text (toString (parse (tokenize(String.words model.content)))) ]
    ]