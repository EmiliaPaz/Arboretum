module Tokenizer exposing (fromMaybeInt, isBoolean, operator, operators, stringToBoolean, stringToInt, tokenize, tokenizeLine, tokenizePrint)

import Debug exposing (toString)
import Types exposing (..)



-------------------------------------- Tokenizer --------------------------------------


operator : String -> Token
operator str =
    case str of
        "" ->
            TokInvalid

        other ->
            if str == "+" then
                TTSC (TTSCInt TokPlus)

            else if str == "-" then
                TTSC (TTSCInt TokMinus)

            else if str == "*" then
                TTSC (TTSCInt TokTimes)

            else if str == "=" then
                TokAssign

            else if str == "==" then
                TTSC (TTSCBool TokEq)

            else if str == "&&" then
                TTSC (TTSCBool TokAnd)

            else if str == "||" then
                TTSC (TTSCBool TokOr)

            else if str == "?" then
                TokHole

            else
                TokInvalid


operators =
    [ "+", "-", "*", "=", "==", "&&", "||", "?" ]


isBoolean : String -> Bool
isBoolean x =
    case x of
        "True" ->
            True

        "False" ->
            True

        other ->
            False


stringToBoolean : String -> Bool
stringToBoolean x =
    case x of
        "True" ->
            True

        "False" ->
            False

        _ ->
            False



-- should never get called


stringToInt : String -> Int
stringToInt x =
    fromMaybeInt (String.toInt x)


fromMaybeInt : Maybe Int -> Int
fromMaybeInt x =
    case x of
        Nothing ->
            0

        -- should never get called
        Just y ->
            y


tokenize : List (List String) -> List (List Token)
tokenize ls =
    List.map tokenizeLine ls


tokenizeLine : List String -> List Token
tokenizeLine str =
    case str of
        [] ->
            []

        x :: xs ->
            if List.member x operators then
                operator x :: tokenizeLine xs

            else if isBoolean x then
                TokConstBool (stringToBoolean x) :: tokenizeLine xs

            else if String.filter Char.isDigit x == x && x /= "" then
                TokConstInt (stringToInt x) :: tokenizeLine xs

            else if String.filter Char.isLower x == x && x /= "" then
                TokVar x :: tokenizeLine xs

            else if x == "(" then
                TokLParen :: tokenizeLine xs

            else if x == ")" then
                TokRParen :: tokenizeLine xs

            else if x == "/" then
                TokSlash :: tokenizeLine xs

            else if x == "->" then
                TokArrow :: tokenizeLine xs

            else if x == "" then
                tokenizeLine xs

            else
                TokInvalid :: tokenizeLine xs


tokenizePrint : List Token -> String
tokenizePrint tokens =
    case tokens of
        [] ->
            ""

        x :: [] ->
            Debug.toString x

        x :: xs ->
            case x of
                _ ->
                    Debug.toString x ++ ", " ++ tokenizePrint xs
