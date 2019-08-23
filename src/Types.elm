module Types exposing (..)

import Dict exposing (Dict)

type Tree a = Node a (List (Tree a))

type Token = TTSC TokTSC | TokAssign | TokLParen | TokRParen | TokHole |
              TokVar String | TokHasType | TokConstInt Int | TokConstBool Bool |
              TokInvalid | TokBackSlash | TokArrow | TokEnd | TokTypeName String

type TokTSC = TTSCInt TokTSCInt | TTSCBool TokTSCBool
type TokTSCInt = TokPlus | TokMinus | TokTimes
type TokTSCBool = TokEq | TokAnd | TokOr

{-
type Const = CBool Bool | CInt Int
type Term = CTerm Const | VTerm String | Plus Term Term | Minus Term Term | Times Term Term
              | Div Term Term | Mod Term Term | Eq Term Term | And Term Term | Or Term Term
              | Lam String Term | App Term Term |  Tuple Term Term | MissingInt | MissingBool | Missing | EmptyTree
-}

type BinOp = Plus | Minus | Times | Div | Mod | Eq | And | Or

type Const = CBool Bool | CInt Int

type Term = CTerm Const | VTerm String | BinTerm BinOp Term Term | Tuple Term Term
            | Lam String Term | App Term Term | Missing | EmptyTree

{-
  V(alue)Type is a type that a TreeAssembly term can evaluate to
-}
type VType = TVar String | TBool | TInt | TFun VType VType | TTuple VType VType

type alias Var =
  { name: String
  , term: Term
  , vtype: VType}

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
