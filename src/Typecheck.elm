module Typecheck exposing (TSubst, CallTree, typeToString, tsubstToString, typecheck, typecheckAll, unify, apply, finalType)

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


andThen : (Maybe TSubst -> List CallTree -> CallTree) -> (TSubst -> Maybe TSubst) -> CallTree -> CallTree
andThen treeConstructor fn (Tree tree) =
  let 
    newSubs =
      case tree.node.subs of
        Just subs ->
          fn subs

        Nothing -> Nothing
  in
    treeConstructor newSubs [Tree tree]


andThen2 : (Maybe TSubst -> List CallTree -> CallTree) -> (TSubst -> TSubst -> Maybe TSubst) -> CallTree -> CallTree -> CallTree
andThen2 treeConstructor fn (Tree tree1) (Tree tree2) =
  let 
    newSubs =
      case (tree1.node.subs, tree2.node.subs) of
        (Just subs1, Just subs2) ->
          fn subs1 subs2

        _ -> Nothing
  in
    treeConstructor newSubs [Tree tree1, Tree tree2]

{-
implementation of algorithm M
-}
typecheck : TypeEnv -> Term -> VType -> List String -> CallTree
typecheck env term ty names =
  let
    makeTree maybeSubs childTrees2 =
      Tree 
        { node = 
          { env = env
          , term = term
          , inType = ty
          , subs = maybeSubs
          }
        , children = childTrees2
        }
    
    andThenIn = andThen makeTree
    andThenIn2 = andThen2 makeTree

    in
      case term of
        CTerm c ->
          case c of
            CInt _ ->
              makeTree (unify ty TInt) []
            CBool _ ->
              makeTree (unify ty TBool) []

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
                  andThenIn2 (\subs1 subs2 -> Just (sub ++ subs1 ++ subs2)) leftTree rightTree

              Nothing ->
                makeTree Nothing []

        VTerm v ->
          case Dict.get v env of
            Just vType -> makeTree (unify ty vType) []
            Nothing    -> makeTree Nothing []

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
                  andThenIn (\fnSubs -> (Just (sub ++ fnSubs))) fnTree
              
              Nothing ->
                makeTree Nothing []
        
        App fn arg ->
          let
            (fnNames, argNames) = split names
            (inName, fnRemainder) = getNext fnNames
            inVar = TVar inName

            fnTree = typecheck env fn (TFun inVar ty) fnRemainder
            argTree = getSubs fnTree |> Maybe.andThen (\sub -> Just (typecheck (applyEnv sub env) arg (apply sub inVar) argNames))

          in
            case argTree of
              Just aTree ->
                andThenIn2 (\fnSubs argSubs -> Just (fnSubs ++ argSubs)) fnTree aTree
              Nothing ->
                andThenIn (\_ -> Nothing) fnTree

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
                  tree1 = typecheck (applyEnv sub env) t1 (apply sub tvar1) names1
                  tree2 = getSubs tree1 |> Maybe.andThen (\sub1 -> Just (typecheck (applyEnv sub1 (applyEnv sub env)) t2 (apply sub1 (apply sub tvar2)) names2))

                in
                  case tree2 of
                    Just justTree2 ->
                      andThenIn2 (\sub1 sub2 -> Just (sub ++ sub1 ++ sub2)) tree1 justTree2
                    Nothing ->
                      andThenIn (\_ -> Nothing) tree1

              Nothing ->
                makeTree Nothing []


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

