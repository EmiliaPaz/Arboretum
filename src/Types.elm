module Types exposing (..)

import Dict exposing (Dict)

type BinOp = Plus | Minus | Times | Div | Mod | Eq | And | Or
type Const = CBool Bool | CInt Int
type Term = CTerm Const | VTerm String | BinTerm BinOp Term Term | Tuple Term Term
            | Lam String Term | App Term Term

{-
  V(alue)Type is a type that a TreeAssembly term can evaluate to
-}
type VType = TVar String | TBool | TInt | TFun VType VType | TTuple VType VType

type alias TermEnv = Dict String Term
type alias TypeEnv = Dict String VType 

{-
  Converts a list of VTypes to a nested VType in the form VFun VType VType
-}
listToTypeSign : List VType -> Maybe VType
listToTypeSign types =
  case types of
    t :: u :: ts ->
      case ts of
        [] -> Just (TFun t u)
        _ ->
          case listToTypeSign ts of
            Just vt -> Just (TFun (TFun t u) (vt))
            Nothing -> Nothing
    t :: ts ->
      case ts of
        [] -> Just t
        _ ->
          case listToTypeSign ts of
            Just vt -> Just (TFun t (vt))
            Nothing -> Nothing
    [] -> Nothing

{-
  Converts a nested VType in the form VFun VType VType to a list of VTypes
-}
typeSignToList : VType -> List VType
typeSignToList vt =
  case vt of
    TVar n          -> [TVar n]
    TBool           -> [TBool]
    TInt            -> [TInt]
    TFun vt1 vt2    -> (typeSignToList vt1) ++ (typeSignToList vt2)
    TTuple vt1 vt2  -> [TTuple vt1 vt2 ]
