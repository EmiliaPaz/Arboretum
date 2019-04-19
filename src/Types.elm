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
            | MissingInt | MissingBool | Missing | EmptyTree

-- V(alue)Type is a type that a TreeAssembly term can evaluate to
{-
  TAny may be used when the type can be either int or bool (discuss)
  TNone is used as a default value for types we haven't determined yet (similar to EmptyTree)
-}
type VType = TBool | TInt | TFun VType VType | TAny | TNone

type alias Var =
  { name: String
  , term: Term
  , vtype: VType}

{-
  Returns the type signature of basic language constructs (if it's not relevant, return TNone)
-}
getTypeSignature : Term -> VType
getTypeSignature t =
  case t of
    CTerm (CBool b) -> TBool
    CTerm (CInt i) -> TInt
    Plus x y -> TFun (TFun TInt TInt) TInt
    Minus x y -> TFun (TFun TInt TInt) TInt
    Times x y -> TFun (TFun TInt TInt) TInt
    Eq x y -> TFun (TFun TInt TInt) TBool
    And x y -> TFun (TFun TBool TBool) TBool
    Or x y -> TFun (TFun TBool TBool) TBool
    Lam x y -> getTypeSignature y --Possibly useful for nested functions
    App x y -> TNone --Might have to change this later on
    _ -> TNone

{-
  Converts a list of VTypes to a nested VType in the form VFun VType VType
-}
listToTypeSign : List VType -> VType
listToTypeSign types =
  case types of
    t :: u :: ts ->
      case ts of
        [] -> TFun t u
        _ -> TFun (TFun t u) (listToTypeSign ts)
    t :: ts ->
      case ts of
        [] -> t
        _ -> TFun t (listToTypeSign ts)
    [] -> TNone

{-
  Converts a nested VType in the form VFun VType VType to a list of VTypes 
-}
typeSignToList : VType -> List VType
typeSignToList vt =
  case vt of
    TBool -> [TBool]
    TInt -> [TInt]
    TAny -> [TAny]
    TNone -> [TNone]
    TFun vt1 vt2 -> (typeSignToList vt1) ++ (typeSignToList vt2)
