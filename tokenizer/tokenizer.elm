import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Operator = Plus | Minus | Times | Assign | Eq | And | Or | Invalid
type Token = TokOp Operator | TokLParen | TokRParen | TokVar String | TokConstInt String | TokConstBool String | TokEnd

operator : String -> Operator
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
booleans = ["T", "F"]
integers = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
variables = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]


toListOfTwo : List Char -> List String
toListOfTwo str =
    case str of
        [] -> []
        (a::b::ls) ->
            if (List.member ((String.fromChar a) ++ (String.fromChar b)) operators)
                then ((String.fromChar a) ++ (String.fromChar b)) :: toListOfTwo ls
            else
                case str of
                    [] -> []
                    (x::xs) ->
                        if ((List.member (String.fromChar x) operators) || (List.member (String.fromChar x) booleans)
                        || (Char.isDigit x) || (Char.isLower x) || (x == '(') || (x == ')'))
                            then (String.fromChar x) :: toListOfTwo xs
                        else if (x == ' ')
                            then toListOfTwo xs
                        else "INVALID" :: toListOfTwo xs
        (a::ls) ->
            if ((List.member (String.fromChar a) operators) || (List.member (String.fromChar a) booleans)
                || (Char.isDigit a) || (Char.isLower a) || (a == '(') || (a == ')'))
                then (String.fromChar a) :: toListOfTwo ls
            else if (a == ' ')
                then toListOfTwo ls
            else "INVALID" :: toListOfTwo ls

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
            (x::xs) -> --TokOp (operator x) :: tokenize xs
                if (List.member x operators)
                --if (List.member x operators)
                    then TokOp (operator x) :: tokenize xs
                else if (isBoolean x)
                --else if (List.member x booleans)
                    then TokConstBool x :: tokenize xs
                --else if (String.filter List.member (x integers) == x)
                else if (String.filter Char.isDigit x == x && x /= "")
                    then TokConstInt x :: tokenize xs
                --else if (String.filter List.member (x variables) == x)
                else if (String.filter Char.isLower x == x && x /= "")
                    then TokVar x :: tokenize xs
                else if (x == "(")
                    then TokLParen :: tokenize xs
                else if (x == ")")
                    then TokRParen :: tokenize xs
                else if (x == "")
                    then tokenize xs
                else TokOp Invalid :: tokenize xs

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
    --, div [] [text (String.join "/" (String.words model.content))]
    , div [] [ text (tokenizePrint(tokenize(String.words model.content))) ]
    --, div [] [ text (tokenizePrint(tokenize( toListOfTwo (String.toList model.content)))) ]
    ]

