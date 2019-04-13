module Evaluate exposing (..)

import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (..)
import Environment exposing (Env, lookup)
import Typecheck exposing(substitute)

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int

boolToString : Bool -> String
boolToString b =
  case b of
    True -> "True"
    False -> "False"


valToString : Maybe Val -> String
valToString v =
  case v of
    Just (VBool x) -> boolToString x
    Just (VInt x)  -> String.fromInt x
    Nothing        -> "Undefined"


termToString : Term -> String
termToString t =
  case t of
    CTerm x ->
      case x of
        CBool a -> boolToString a
        CInt a -> String.fromInt a

    VTerm x ->
      x

    Plus t1 t2 ->
      "(" ++ (termToString t1) ++ " + " ++ (termToString t2) ++ ")"

    Minus t1 t2 ->
      "(" ++ (termToString t1) ++ " - " ++ (termToString t2) ++ ")"

    Times t1 t2 ->
      "(" ++ (termToString t1) ++ " * " ++ (termToString t2) ++ ")"

    Eq t1 t2 ->
      "(" ++ (termToString t1) ++ " == " ++ (termToString t2) ++ ")"

    And t1 t2 ->
      "(" ++ (termToString t1) ++ " && " ++ (termToString t2) ++ ")"

    Or t1 t2 ->
      "(" ++ (termToString t1) ++ " || " ++ (termToString t2) ++ ")"

    Lam t1 t2 ->
      "(\\ " ++ (termToString t1) ++ " -> " ++ (termToString t2) ++ ")"

    App t1 t2 ->
      "(" ++ (termToString t1) ++ (termToString t2) ++ ")"

    _ -> ""


tryBinFn : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
tryBinFn f mx my =
  case mx of
    Just x ->
      case my of
        Just y  -> Just (f x y)
        Nothing -> Nothing

    Nothing -> Nothing

tryBool : Maybe Val -> Maybe Bool
tryBool mx =
  case mx of
    Just (VBool x) -> Just x
    _              -> Nothing

tryInt : Maybe Val -> Maybe Int
tryInt mx =
  case mx of
    Just (VInt x) ->  Just x
    _              -> Nothing

wrapInt : Maybe Int -> Maybe Val
wrapInt c =
  case c of
    Just x  -> Just (VInt x)
    Nothing -> Nothing

wrapBool : Maybe Bool -> Maybe Val
wrapBool c =
  case c of
    Just x  -> Just (VBool x)
    Nothing -> Nothing

-- an approximaton of an 'or' operation with maybe
takeOne : (Maybe a, Maybe a) -> Maybe a
takeOne (mx, my) =
  case mx of
    Just x  -> Just x
    Nothing ->
      case my of
        Just y ->  Just y
        Nothing -> Nothing


-- evaluates a term
eval : Env -> Term -> Maybe Val
eval e t =
  let
    evale = eval e
  in
  case t of
    CTerm c ->
      case c of
        CInt x  -> Just (VInt x)
        CBool x -> Just (VBool x)

    VTerm v ->
      case lookup e v of
        Just subst -> evale subst
        Nothing    -> Nothing

    Plus x y ->
      wrapInt ( tryBinFn (+) (tryInt (evale x)) (tryInt (evale y)) )

    Minus x y ->
      wrapInt ( tryBinFn (-) (tryInt (evale x)) (tryInt (evale y)) )

    Times x y ->
      wrapInt ( tryBinFn (*) (tryInt (evale x)) (tryInt (evale y)) )

    Eq x y ->
      takeOne
        ( wrapBool ( tryBinFn (==) (tryInt (evale x)) (tryInt (evale y)) )
        , wrapBool ( tryBinFn (==) (tryBool (evale x)) (tryBool (evale y)) )
        )

    And x y ->
      wrapBool ( tryBinFn (&&) (tryBool (evale x)) (tryBool (evale y)) )

    Or x y ->
      wrapBool ( tryBinFn (||) (tryBool (evale x)) (tryBool (evale y)) )

    Lam x y ->
      evale y

    App x y ->
      case x of
        Lam w z ->
          case w of
            VTerm n -> evale (substitute z y n)
            _ -> Nothing
        VTerm v ->
          let lambda = lookup e v
            in case lambda of
              Just (Lam w z) ->
                case w of
                  VTerm n -> evale (substitute z y n)
                  _ -> Nothing
              _ -> Nothing
        _ -> Nothing
    _ -> Nothing
