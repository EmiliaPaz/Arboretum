module Types exposing (..)

type alias Var = 
  { name: String
  , term: Term }


type Token = TokPlus | TokMinus | TokTimes | TokAssign | TokEq | TokAnd | TokOr | TokLParen | TokRParen | TokVar String | TokConstInt Int | TokConstBool Bool | TokInvalid | TokEnd

type Term = CTerm Const | VTerm Var | Plus Term Term | Minus Term Term | Times Term Term | Eq Term Term | And Term Term | Or Term Term | EmptyTree
type Const = CBool Bool | CInt Int

-- V(alue)Type is a type that a TreeAssembly term can evaluate to
type VType = TBool | TInt

-- Val is a value that a term can evaluate to
type Val = VBool Bool | VInt Int