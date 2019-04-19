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
type VType = TBool | TInt | TFun VType VType | TAny | TNone

type alias Var =
  { name: String
  , term: Term
  , vtype: VType}


getTypeSignature : Term -> VType
getTypeSignature t =
  case t of
    CTerm (CBool b) -> TBool
    CTerm (CInt i) -> TInt
    VTerm s -> TNone
      -- case lookup s of
      --   Just term -> getTypeSignature term --assuming it already exists
        -- Nothing -> TNone
    Plus x y -> TFun (TFun TInt TInt) TInt
    Minus x y -> TFun (TFun TInt TInt) TInt
    Times x y -> TFun (TFun TInt TInt) TInt
    Eq x y -> TFun (TFun TInt TInt) TBool
    And x y -> TFun (TFun TBool TBool) TBool
    Or x y -> TFun (TFun TBool TBool) TBool
    Lam x y -> TNone --Temporarily
    App x y -> TNone --Temporarily
    _ -> TNone

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
