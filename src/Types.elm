module Types exposing (..)

type Tree a = Node a (List (Tree a))

type Token = TTSC TokTSC | TokAssign | TokLParen | TokRParen | TokHole | TokVar String
             | TokConstInt Int | TokConstBool Bool | TokInvalid | TokSlash | TokArrow | TokEnd

type TokTSC = TTSCInt TokTSCInt | TTSCBool TokTSCBool
type TokTSCInt = TokPlus | TokMinus | TokTimes
type TokTSCBool = TokEq | TokAnd | TokOr

type Const = CBool Bool | CInt Int
type Term = CTerm Const | VTerm String | Plus Term Term | Minus Term Term | Times Term Term
            | Eq Term Term | And Term Term | Or Term Term | Lam Term Term | MissingInt | MissingBool | Missing | EmptyTree

-- V(alue)Type is a type that a TreeAssembly term can evaluate to
type VType = TBool | TInt

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int

type alias Var =
  { name: String
  , term: Term }

type alias Env = (String -> Maybe Term)

{-
CheckResult represents the outcome of typechecking a term

Checks type : The term successfully typechecks to `type`
Fails argNum expected got output : The terms fails typechecking, where
  `argNum` was of type `got` instead of `expected`.  The term would have
  output type `output`, had typechecking succeeded.
Partial type : At the top level, the term typehcecking was successful with
  `type`, but an error occured somewhere in the derivation tree
Invalid : Typechecking failed with no useful diagnostic info
-}
type CheckResult = Checks VType | Fails Int VType VType VType | Partial VType | Invalid
