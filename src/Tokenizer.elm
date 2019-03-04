module Tokenizer exposing (..)


import Debug exposing (toString)
import Types exposing (..)

-------------------------------------- Tokenizer --------------------------------------

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

stringToBoolean : String -> Bool
stringToBoolean x =
  case x of
      "True"  -> True
      "False" -> False
      _       -> False                                    -- should never get called

stringToInt : String -> Int
stringToInt x = fromMaybeInt(String.toInt x)

fromMaybeInt : Maybe Int -> Int
fromMaybeInt x = case x of
                  Nothing -> 0                            -- should never get called
                  Just y  -> y


tokenize : List String -> List Token
tokenize str =
        case str of
            [] -> []
            (x::xs) ->
                if (List.member x operators)
                    then (operator x) :: tokenize xs
                else if (isBoolean x)
                    then TokConstBool (stringToBoolean x) :: tokenize xs
                else if (String.filter Char.isDigit x == x && x /= "")
                    then TokConstInt (stringToInt x) :: tokenize xs
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
        (x::[]) -> (Debug.toString x)
        (x::xs) ->
            case x of
                _ -> (Debug.toString x) ++ ", " ++ tokenizePrint xs
