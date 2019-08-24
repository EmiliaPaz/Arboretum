module Typecheck exposing (CheckResult(..), CheckEnv, CheckNode, CheckTree, TSubst, CallTree, typeToString, tsubstToString, checkResultToString, typecheck, typecheckAll, unify, apply, finalType)

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


{-andThen : (VType -> CheckResult) -> CheckResult -> CheckResult
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
      , children = [tree1, tree2] } -}


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

finalType : CallTree -> Maybe VType
finalType (Tree tree) =
  tree.node.subs |> Maybe.andThen (\s ->
    Just (apply s (TVar "t0"))
  )


andThen : (TSubst -> CallTree) -> Maybe CallTree -> Maybe CallTree
andThen fn tree =
  case tree of
  case tree.node.subs of
    Just s  -> fn s
    Nothing -> Nothing

{-
implementation of algorithm M
-}
typecheck : TypeEnv -> Term -> VType -> List String -> CallTree
typecheck env term ty names =
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
                  leftTree = typecheck env t1 opType names
                  rightTree = typecheck env t2 opType names
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
                  fnTree = typecheck (applyEnv sub (Dict.insert argName argVar env)) body (apply sub bodyVar) remainder2
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

            fnTree = typecheck env fn (TFun inVar ty) fnRemainder
            argTree = getSubs fnTree |> Maybe.andThen (\sub -> Just (typecheck (applyEnv sub env) arg (apply sub inVar) argNames))

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

        Tuple t1 t2 ->
          let
            (names1, names2) = split names
            (name1, remainder1) = getNext names1
            (name2, remainder2) = getNext names2
            tvar1 = TVar name1
            tvar2 = TVar name2
          in
            case unify ty (TTuple tvar1 tvar2) of
              Just sub ->
                let 
                  typecheck (applyEnv sub env) t1 (apply sub tvar1) names1 |> Just |> Maybe.andThen (\sub1 ->
                    typecheck (applyEnv sub1 (applyEnv sub env)) t1 (apply sub1 (apply sub tvar2)) names2 |> Just |> Maybe.andThen (\sub2 ->
                  )

                  (leftTree, rightTree) = 
                    typecheck (applyEnv sub env) t1 (apply sub tvar1) names
                      |> ((Tree lTree) -> lTree.node.subs |> Maybe.andThen (\subs ->
                            typecheck (applyEnv)
                         )
                  rightTree = typecheck env t2 opType names
                in
                  case (getSubs leftTree, getSubs rightTree) of
                    (Just ls, Just rs) ->
                      (Just (sub ++ ls ++ rs), [leftTree, rightTree])
                    _ ->
                      (Nothing, [leftTree, rightTree])

              Nothing ->
                (Nothing, [])

          {-let
            (names1, names2) = split names
            (name1, remainder1) = getNext names1
            (name2, remainder2) = getNext names2
            tvar1 = TVar name1
            tvar2 = TVar name2
          in
            unify ty (TTuple tvar1 tvar2) |> (\tree ->
              typecheck (applyEnv sub env) t1 tvar1 names1 |> Maybe.andThen (\sub1 ->
                typecheck (applyEnv sub1 (applyEnv sub env)) t2 tvar2 names2 |> Maybe.andThen (\sub2 ->
                  Just (sub ++ sub1 ++ sub2)
                )
              )
            )-}

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


indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl fn first xs =
  List.foldl (\val (acc, i) -> (fn i val acc, i + 1)) (first, 0) xs
    |> Tuple.first

tvarNames =
  List.range 1 1000
    |> map String.fromInt
    |> map (\n -> "t" ++ n)


renameTVars : Int -> VType -> VType
renameTVars n ty =
  case ty of
    TInt -> TInt
    TBool -> TBool
    TVar name -> TVar (name ++ "." ++ String.fromInt n)
    TFun ty1 ty2 -> TFun (renameTVars n ty1) (renameTVars n ty2)
    TTuple ty1 ty2 -> TTuple (renameTVars n ty1) (renameTVars n ty2)


typecheckAll : List (String, Term) -> List CallTree
typecheckAll ts =
  indexedFoldl (\i (varName, term) (env, trees) ->
    let
      tree = typecheck env term (TVar "t0") tvarNames
      ty = case tree of
        Tree t -> t.node.subs |> Maybe.andThen 
          (\s ->
            apply s (TVar "t0")
              |> renameTVars i 
              |> Just )
          
    in
      case ty of
        Just valid ->
          (Dict.insert varName valid env, trees ++ [tree])
        
        Nothing ->
          (env, trees ++ [tree])

  ) (Dict.empty, []) ts
    |> Tuple.second

