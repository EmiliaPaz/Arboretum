import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

--type Operator = Plus | Minus | Times | Assign | Eq | And | Or | Invalid
type Token = Plus | Minus | Times | Assign | Eq | And | Or | TokLParen | TokRParen | TokVar String | TokConstInt String | TokConstBool String | Invalid | TokEnd

operator : String -> Token
operator str =
            case str of
                "" -> Invalid
                other ->
                    if (str == "+")
                        then Plus
                     else if (str == "-")
                         then Minus
                     else if (str == "*")
                         then Times
                     else if (str == "=")
                         then Assign
                     else if (str == "==")
                         then Eq
                     else if (str == "&&")
                         then And
                     else if (str == "||")
                         then Or
                     else Invalid

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
                else Invalid :: tokenize xs

tokenizePrint : List Token -> String
tokenizePrint tokens =
    case tokens of
        [] -> ""
        (x::xs) ->
            case x of
                _ -> (Debug.toString x) ++ ", " ++ tokenizePrint xs


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



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to tokenize", value model.content, onInput Change ] []
    , div [] [ text (tokenizePrint(tokenize(String.words model.content))) ]
    ]
