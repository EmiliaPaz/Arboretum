module Types exposing (..)

type Tree a = Node a (List (Tree a))

type Token = TTSC TokTSC | TokAssign | TokLParen | TokRParen | TokHole |
              TokVar String | TokHasType | TokConstInt Int | TokConstBool Bool |
              TokInvalid | TokBackSlash | TokArrow | TokEnd | TokTypeName String

type TokTSC = TTSCInt TokTSCInt | TTSCBool TokTSCBool
type TokTSCInt = TokPlus | TokMinus | TokTimes
type TokTSCBool = TokEq | TokAnd | TokOr

type Const = CBool Bool | CInt Int
type Term = CTerm Const | VTerm String | Plus Term Term | Minus Term Term | Times Term Term
            | Eq Term Term | And Term Term | Or Term Term | Lam String Term | App Term Term
            | MissingInt | MissingBool | Missing | EmptyTree | Tuple Term Term

{- 
  V(alue)Type is a type that a TreeAssembly term can evaluate to
-}
type VType = TBool | TInt | TFun VType VType | TTuple VType VType

type alias Var =
  { name: String
  , term: Term
  , vtype: VType}

{-
  Returns the type signature of basic language constructs (only used for binary operators)
-}
getTypeSignature : Term -> Maybe VType
getTypeSignature t =
  case t of
    CTerm (CBool b) -> Just TBool
    CTerm (CInt i) -> Just TInt
    Plus x y -> Just (TFun TInt (TFun TInt TInt))
    Minus x y -> Just (TFun TInt (TFun TInt TInt))
    Times x y -> Just (TFun TInt (TFun TInt TInt))
    Eq x y -> Just (TFun TInt (TFun TInt TBool))
    And x y -> Just (TFun TBool (TFun TBool TBool))
    Or x y -> Just (TFun TBool (TFun TBool TBool))
    Lam x y -> Nothing --Need to work on this.
    App x y -> Nothing --Might have to change this later on
    _ -> Nothing

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
    TBool           -> [TBool]
    TInt            -> [TInt]
    TFun vt1 vt2    -> (typeSignToList vt1) ++ (typeSignToList vt2)
    TTuple vt1 vt2  -> (typeSignToList vt1) ++ (typeSignToList vt2)   -- Will return the tuple as a list. Not ideal, but the function is type sign to list
