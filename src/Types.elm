module Types exposing (..)

type Tree a = Node a (List (Tree a))

type Token = TTSC TokTSC | TokAssign | TokLParen | TokRParen | TokHole | TokVar String
             | TokConstInt Int | TokConstBool Bool | TokInvalid | TokBackSlash | TokArrow | TokEnd

type TokTSC = TTSCInt TokTSCInt | TTSCBool TokTSCBool
type TokTSCInt = TokPlus | TokMinus | TokTimes
type TokTSCBool = TokEq | TokAnd | TokOr

type Const = CBool Bool | CInt Int
type Term = CTerm Const | VTerm String | Plus Term Term | Minus Term Term | Times Term Term
            | Eq Term Term | And Term Term | Or Term Term | Lam Term Term | App Term Term
            | MissingInt | MissingBool | Missing | EmptyTree | Tuple Term Term

type alias Var =
  { name: String
  , term: Term }

-- elm repl
--   env = [("x",CTerm (CInt 4)),("y",Lam (VTerm "x") (Plus (VTerm "x") (CTerm (CInt 1)))),("a",App (VTerm "y") (CTerm (CInt 2))),("b",Plus (CTerm (CInt 1)) (CTerm (CInt 2)))] 
