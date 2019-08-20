module Typecheck exposing (CheckResult(..), CheckEnv, CheckNode, CheckTree, typeToString, checkResultToString, typecheck, typecheckAll)

import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Dict exposing (Dict)

import Stack exposing (Stack)
import Tree exposing (Tree(..))
import Types exposing (Const(..), BinOp(..), Term(..), VType(..), TermEnv, listToTypeSign, typeSignToList)



typeToString : VType -> String
typeToString t =
  case t of
    TBool -> "Bool"
    TInt  -> "Int"
    TTuple a b -> "Tuple" ++ " " ++ (typeToString a) ++ " " ++ (typeToString b)
    TFun a b -> (typeToString a) ++ " -> " ++ (typeToString b)

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

type alias CheckEnv = Dict String CheckResult

type alias CheckNode =
  { term: Term
  , check: CheckResult }

type alias CheckTree = Tree CheckNode

type alias CheckStack = Stack CheckTree

checkResultToString : CheckResult -> String
checkResultToString r =
  case r of
    Checks t ->
      typeToString t

    Fails argNum exp got out ->
      "Fails " ++ typeToString out

    Partial t ->
      "Partial " ++ typeToString t

    Invalid ->
      "Invalid"


andThen : (VType -> CheckResult) -> CheckResult -> CheckResult
andThen fn result =
  case result of
    Checks t1 ->
      fn t1
    
    Partial t1 ->
      case fn t1 of
        Checks t2 -> Partial t2
        _         -> fn t1
    
    Fails _ _ _ out ->
      case fn out of
        Checks t2 -> Partial t2
        _         -> fn out

    Invalid -> Invalid


andThen2 : (VType -> VType -> CheckResult) -> CheckResult -> CheckResult -> CheckResult
andThen2 fn res1 res2 =
  case res1 of
    Checks t1 ->
      andThen (fn t1) res2

    Partial t1 ->
      case andThen (fn t1) res2 of
        Checks t2 -> Partial t2
        _         -> andThen (fn t1) res2

    Fails _ _ _ out ->
      case andThen (fn out) res2 of
        Checks t2 -> Partial t2
        _         -> andThen (fn out) res2
    
    Invalid -> Invalid

andThenTree : (VType -> CheckResult) -> Term -> CheckTree -> CheckTree
andThenTree fn term tree =
  let 
    result = case tree of
      Tree t -> andThen fn t.node.check
  in
    Tree
      { node =
        { term = term
        , check = result }
      , children = [tree] }


andThenTree2 : (VType -> VType -> CheckResult) -> Term -> CheckTree -> CheckTree -> CheckTree
andThenTree2 fn term tree1 tree2 =
  let 
    result = case (tree1, tree2) of
      (Tree t1, Tree t2) -> andThen2 fn t1.node.check t2.node.check
  in
    Tree
      { node =
        { term = term
        , check = result }
      , children = [tree1, tree2] }


checkBinOp : CheckEnv -> BinOp -> VType -> VType -> CheckResult
checkBinOp env op t1 t2 =
  let
    operandType =
      case op of
        Plus -> TInt
        Minus -> TInt
        Times -> TInt
        Div -> TInt
        Mod -> TInt
        Eq -> TInt
        And -> TBool
        Or -> TBool

    resultType =
      case op of
        Plus -> TInt
        Minus -> TInt
        Times -> TInt
        Div -> TInt
        Mod -> TInt
        Eq -> TBool
        And -> TBool
        Or -> TBool
  in
    if t1 == operandType then
      if t2 == operandType then
        Checks resultType
      else
        Fails 2 operandType t2 resultType
    else
      Fails 1 operandType t1 resultType


getCheck : CheckTree -> CheckResult
getCheck tree = case tree of
  Tree t -> t.node.check


singletonTree : Term -> CheckResult -> CheckTree
singletonTree term result =
  Tree
    { node =
      { term = term
      , check = result }
    , children = []}
    

{-
Checks the type of a term in the context of an environment and argument stack.

Why do we return a CheckTree?
The way typechecking functions is implemented, functions can only recieve
types if we know the argument type.  For this reason functions only have types
in the greater context of their surrounding applications, and we need to
preserve this information when typechecking the largeer expression.

Argument stack?
The typechecking process keeps track of an argument stack.  Each application
pushes its argument on to the stack, and each function pops an argument off
the stack.  This is another consequence of the weirdness of typechecking
without Hindley-Milner.
-}
typecheck : CheckEnv -> CheckStack -> Term -> CheckTree
typecheck env argStack t =
  case t of
    CTerm c ->
      case c of
        CInt _  -> singletonTree t (Checks TInt)
        CBool _ -> singletonTree t (Checks TBool)

    BinTerm op t1 t2 ->
      let
        type1 = typecheck env argStack t1
        type2 = typecheck env argStack t2
      in
        andThenTree2 (\ty1 ty2 -> checkBinOp env op ty1 ty2) t type1 type2
    
    VTerm v ->
      case Dict.get v env of
        Just sub -> singletonTree t sub
        Nothing  -> singletonTree t Invalid
    
    Lam name body ->
      let
        argTree = 
          case Stack.peek argStack of
            Just sub -> sub
            Nothing  -> singletonTree (VTerm name) Invalid
        outTree = typecheck (Dict.insert name (getCheck argTree) env) (Stack.pop argStack) body
      in
        andThenTree2 (\a o -> Checks (TFun a o)) t argTree outTree
        
    App fn arg ->
      let
        argTree = typecheck env argStack arg
        fnTree = typecheck env (Stack.push argTree argStack) fn
      in
        andThenTree2
          (\ft at -> case ft of
            TFun it ot -> Checks ot
            _          -> Invalid) t fnTree argTree

    Tuple t1 t2 ->
      andThenTree2 (\c1 c2 -> Checks (TTuple c1 c2)) t (typecheck env argStack t1) (typecheck env argStack t2)
      
    _ -> singletonTree t Invalid


{-
Typechecks a list of terms in the order they appear in, accumulating the
check environment as it goes.
-}
typecheckAll : List (String, Term) -> CheckEnv
typecheckAll ts =
  List.foldl ( \(name,term) env -> Dict.insert name (getCheck (typecheck env Stack.empty term)) env) 
    Dict.empty ts

