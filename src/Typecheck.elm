module Typecheck exposing (CheckResult(..), CheckEnv, CheckNode, CheckTree, TSubst, CallTree, typeToString, tsubstToString, checkResultToString, typecheck, typecheckAll, unify, check, apply)

import List exposing (..)
import List.Extra exposing (elemIndex, getAt, unique)
import Dict exposing (Dict)

import Stack exposing (Stack)
import Tree exposing (Tree(..))
import Types exposing (Const(..), BinOp(..), Term(..), VType(..), TermEnv, TypeEnv, listToTypeSign, typeSignToList)



typeToString : VType -> String
typeToString t =
  case t of
    TBool      -> "Bool"
    TInt       -> "Int"
    TVar name  -> name
    TTuple a b -> "Tuple" ++ " " ++ (typeToString a) ++ " " ++ (typeToString b)
    TFun a b   -> (typeToString a) ++ " -> " ++ (typeToString b)

tsubstToString : TSubst -> String
tsubstToString subst =
  subst
    |> List.map (\(name, sub) -> "(" ++ name ++ ": " ++ typeToString sub ++ ") ")
    |> List.foldr (++) ""

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


type alias TSubst = List (String, VType)


getOpType : BinOp -> VType
getOpType op =
  case op of
    Plus -> TInt
    Minus -> TInt
    Times -> TInt
    Div -> TInt
    Mod -> TInt
    Eq -> TInt
    And -> TBool
    Or -> TBool

getResultType : BinOp -> VType
getResultType op =
  case op of
    Plus -> TInt
    Minus -> TInt
    Times -> TInt
    Div -> TInt
    Mod -> TInt
    Eq -> TBool
    And -> TBool
    Or -> TBool

check : Term -> (CallTree, Maybe VType)
check term =
  let
    names =
      List.range 1 1000
        |> map String.fromInt
        |> map (\n -> "t" ++ n)
  
    tree = typecheck2 Dict.empty term (TVar "t0") names
  in
    ( tree
    , tree 
      |> getSubs
      |> Maybe.andThen (\subs -> Just (apply subs (TVar "t0")))
    )


type alias CallNode =
  { env: TypeEnv
  , term: Term
  , inType: VType
  , subs: Maybe TSubst
  }

type alias CallTree = Tree CallNode

getSubs : CallTree -> Maybe TSubst
getSubs (Tree t) =
  t.node.subs

{-
implementation of algorithm M
-}
typecheck2 : TypeEnv -> Term -> VType -> List String -> CallTree
typecheck2 env term ty names =
  let
    (subs, childTrees) =
      case term of
        CTerm c ->
          case c of
            CInt _ ->
              (unify ty TInt, [])
            CBool _ ->
              (unify ty TBool, [])

        BinTerm op t1 t2 ->
          let
            opType = getOpType op
            resType = getResultType op
          in
            {- TODO: fix environments to carry new substitutions -}
            case unify ty resType of
              Just sub ->
                let 
                  leftTree = typecheck2 env t1 opType names
                  rightTree = typecheck2 env t2 opType names
                in
                  case (getSubs leftTree, getSubs rightTree) of
                    (Just ls, Just rs) ->
                      (Just (sub ++ ls ++ rs), [leftTree, rightTree])
                    _ ->
                      (Nothing, [leftTree, rightTree])

              Nothing ->
                (Nothing, [])

        VTerm v ->
          case Dict.get v env of
            Just vType -> (unify ty vType, [])
            Nothing    -> (Nothing, [])

        Lam argName body ->
          let
            (argVarName, remainder) = getNext names
            (bodyName, remainder2) = getNext remainder
            argVar = TVar argVarName
            bodyVar = TVar bodyName
          in
            case unify ty (TFun argVar bodyVar) of
              Just sub ->
                let
                  fnTree = typecheck2 (applyEnv sub (Dict.insert argName argVar env)) body (apply sub bodyVar) remainder2
                in
                  ( fnTree
                    |> getSubs
                    |> Maybe.andThen (\fnSubs -> (Just (sub ++ fnSubs)))
                  , [fnTree])
              
              Nothing ->
                (Nothing, [])
        
        App fn arg ->
          let
            (fnNames, argNames) = split names
            (inName, fnRemainder) = getNext fnNames
            inVar = TVar inName

            fnTree = typecheck2 env fn (TFun inVar ty) fnRemainder
            argTree = getSubs fnTree |> Maybe.andThen (\sub -> Just (typecheck2 (applyEnv sub env) arg (apply sub inVar) argNames))

            appSubs = 
              case (getSubs fnTree, argTree |> Maybe.andThen getSubs) of
                (Just fnSubs, Just argSubs) ->
                  Just (fnSubs ++ argSubs)
                
                _ ->
                  Nothing

            trees = case argTree of
              Just t  -> [fnTree, t]
              Nothing -> [fnTree]
          in
            (appSubs, trees)

        {-Tuple t1 t2 ->
          let
            (names1, names2) = split names
            (name1, remainder1) = getNext names1
            (name2, remainder2) = getNext names2
            tvar1 = TVar name1
            tvar2 = TVar name2
          in
            unify ty (TTuple tvar1 tvar2) |> Maybe.andThen (\sub ->
              typecheck2 (applyEnv sub env) t1 tvar1 names1 |> Maybe.andThen (\sub1 ->
                typecheck2 (applyEnv sub1 (applyEnv sub env)) t2 tvar2 names2 |> Maybe.andThen (\sub2 ->
                  Just (sub ++ sub1 ++ sub2)
                )
              )
            )-}
        
        _ -> (Nothing, [])

  in
    Tree 
      { node = 
        { env = env
        , term = term
        , inType = ty
        , subs = subs
        }
      , children = childTrees
      }

{-
Deplorable hack.  Will fix when I get the chance.
-}
getNext : List String -> (String, List String)
getNext names =
  case names of
    [] ->
      ("OUT OF NAMES", [])
    x::xs ->
      (x, xs)


split : List a -> (List a, List a)
split vals =
  case vals of
    [] ->
      ([], [])
    
    [x] ->
      ([x], [])

    x::y::zs ->
      let
        (xs, ys) = split zs
      in
        ([x] ++ xs, [y] ++ ys)

lookup : String -> List (String, a) -> Maybe a
lookup name xs =
  xs
    |> List.filter (\(key, _) -> key == name)
    |> List.map (\(_, val) -> val)
    |> List.Extra.getAt 0


apply : TSubst -> VType -> VType
apply subst ty =
  case ty of
    TVar name -> 
      case lookup name subst of
        Just s  -> apply subst s
        Nothing -> ty

    TInt -> TInt
    TBool -> TBool
    TFun x y -> TFun (apply subst x) (apply subst y)
    TTuple x y -> TTuple (apply subst x) (apply subst y)


applyEnv : TSubst -> TypeEnv -> TypeEnv
applyEnv subst env =
  Dict.map (\_ ty -> apply subst ty) env


tvs : VType -> List String
tvs ty =
  case ty of
    TVar name -> [name]
    TBool  -> []
    TInt   -> []
    TFun x y -> unique (tvs x ++ tvs y)
    TTuple x y -> unique (tvs x ++ tvs y)


unifyBin : VType -> VType -> VType -> VType -> Maybe TSubst
unifyBin x1 y1 x2 y2 =
  unify x1 x2 |> Maybe.andThen (\subX ->
    unify (apply subX y1) (apply subX y2) |> Maybe.andThen (\subY ->
      Just (subX ++ subY)
    )
  )


unify : VType -> VType -> Maybe TSubst
unify ty1 ty2 =
  case (ty1, ty2) of
    (TVar v, TVar w) ->
      if v == w then
        Just []
      else
        Just [(v, ty2)]
    
    (TVar v, _) ->
      if not (List.member v (tvs ty2)) then
        Just [(v, ty2)]
      else
        Nothing
    
    (_, TVar w) ->
      if not (List.member w (tvs ty1)) then
        Just [(w, ty1)]
      else
        Nothing

    (TInt, TInt) ->
      Just []
    
    (TBool, TBool) ->
      Just []

    (TFun x1 y1, TFun x2 y2) ->
      unifyBin x1 y1 x2 y2
    
    (TTuple x1 y1, TTuple x2 y2) ->
      unifyBin x1 y1 x2 y2
    
    (_, _) ->
      Nothing

{-
Typechecks a list of terms in the order they appear in, accumulating the
check environment as it goes.
-}
typecheckAll : List (String, Term) -> CheckEnv
typecheckAll ts =
  List.foldl ( \(name,term) env -> Dict.insert name (getCheck (typecheck env Stack.empty term)) env) 
    Dict.empty ts

