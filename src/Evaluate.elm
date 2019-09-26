module Evaluate exposing (..)

import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (..)
import Dict exposing (get, insert)

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int | VFun TermEnv String Term | VTuple Val Val 

boolToString : Bool -> String
boolToString b =
  case b of
    True -> "True"
    False -> "False"


valToString : Maybe Val -> String
valToString v =
  case v of
    Just (VBool x)    -> boolToString x
    Just (VInt x)     -> String.fromInt x
    Just (VFun e n t) ->  "\\" ++ n ++ " -> " ++ (termToString t)
    Just (VTuple x y) -> 
      let x2 = Just x
          y2 = Just y
      in "(" ++ (valToString x2) ++ "," ++ (valToString y2) ++ ")"
    Nothing           -> "Undefined"


termToString : Term -> String
termToString t =
  case t of
    CTerm x ->
      case x of
        CBool a -> boolToString a
        CInt a -> String.fromInt a

    VTerm x ->
      x

    BinTerm op t1 t2 ->
      case op of
        Plus ->
          "(" ++ (termToString t1) ++ " + " ++ (termToString t2) ++ ")"

        Minus ->
          "(" ++ (termToString t1) ++ " - " ++ (termToString t2) ++ ")"

        Times ->
          "(" ++ (termToString t1) ++ " * " ++ (termToString t2) ++ ")"

        Div ->
          "(" ++ (termToString t1) ++ " / " ++ (termToString t2) ++ ")"

        Mod ->
          "(" ++ (termToString t1) ++ " % " ++ (termToString t2) ++ ")"

        Eq ->
          "(" ++ (termToString t1) ++ " == " ++ (termToString t2) ++ ")"

        And ->
          "(" ++ (termToString t1) ++ " && " ++ (termToString t2) ++ ")"

        Or ->
          "(" ++ (termToString t1) ++ " || " ++ (termToString t2) ++ ")"

    Lam t1 t2 ->
      "(\\" ++ t1 ++ " -> " ++ (termToString t2) ++ ")"

    App t1 t2 ->
      "(" ++ (termToString t1) ++ " " ++ (termToString t2) ++ ")"
    
    Tuple t1 t2 ->
      "(" ++ (termToString t1) ++ "," ++ (termToString t2) ++ ")"

    If cond first second ->
      "(if " ++ (termToString cond) ++ " then " ++ (termToString first) ++ " else " ++ (termToString second) ++ ")"


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

valToTerm : Val -> Term
valToTerm v =
  case v of
    VBool b -> CTerm (CBool b)
    VInt i -> CTerm (CInt i)
    VFun e s t -> Lam s t
    VTuple x y -> Tuple (valToTerm x) (valToTerm y)

-- evaluates a term
eval : TermEnv -> Term -> Maybe Val
eval e t =
  let
    evale = eval e
  in
  case t of
    CTerm c ->
      case c of
        CInt x  -> Just (VInt x)
        CBool x -> Just (VBool x)

    VTerm name ->
      case get name e of
        Just subst -> evale subst
        Nothing    -> Nothing
    
    BinTerm op x y ->
      case op of
        Plus ->
          wrapInt ( tryBinFn (+) (tryInt (evale x)) (tryInt (evale y)) )

        Minus ->
          wrapInt ( tryBinFn (-) (tryInt (evale x)) (tryInt (evale y)) )

        Times ->
          wrapInt ( tryBinFn (*) (tryInt (evale x)) (tryInt (evale y)) )

        Div ->
          wrapInt ( tryBinFn (//) (tryInt (evale x)) (tryInt (evale y)) )

        Mod ->
          wrapInt ( tryBinFn (modBy) (tryInt (evale x)) (tryInt (evale y)) )

        Eq ->
          takeOne
            ( wrapBool ( tryBinFn (==) (tryInt (evale x)) (tryInt (evale y)) )
            , wrapBool ( tryBinFn (==) (tryBool (evale x)) (tryBool (evale y)) )
            )

        And ->
          wrapBool ( tryBinFn (&&) (tryBool (evale x)) (tryBool (evale y)) )

        Or ->
          wrapBool ( tryBinFn (||) (tryBool (evale x)) (tryBool (evale y)) )

    -- Lambda gets evaluated to a closure.
    
    Tuple x y -> 
          case (evale x, evale y) of
            (Just x2, Just y2) -> Just (VTuple x2 y2)
            _ -> Nothing

    Lam name body -> Just (VFun e name body)

    App fn arg ->
      case evale fn of
        Just (VFun env name body) ->
          let e2 = insert name arg env in
          eval e2 body
        
        _ ->
          Nothing
    
    If b x y ->
      case evale b of
        Just (VBool True) ->
          evale x

        Just (VBool False) ->
          evale y

        _ -> Nothing