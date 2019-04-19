module Typecheck exposing (CheckResult(..), typeToString, checkResultToString, typecheck, typecheck2, substitute)
import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (Const(..), Term(..), VType(..), getTypeSignature, listToTypeSign)
import Environment exposing (Env, lookup, lookupType, lookupName, extend, replaceVar)



typeToString : VType -> String
typeToString t =
  case t of
    TBool -> "Bool"
    TInt  -> "Int"
    TFun a b -> (typeToString a) ++ " -> " ++ (typeToString b)
    TAny -> "Any"
    TNone -> "None"

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


-- checks a function signature `sig` against a list of argument types `args`
checkSig : List VType -> List CheckResult -> CheckResult
checkSig sig args =
  let
    outTypes =
      map
        (\x ->
          case x of
            Checks t      -> Just t
            Fails _ _ _ t -> Just t
            Partial t     -> Just t
            Invalid       -> Nothing
        )
        args

    checks = map2 (\x y ->
                    case y of
                      Just y2  -> x == y2
                      Nothing -> True
                  ) sig outTypes
    {- this line is not correct! it should be `drop (length args) sig`, but
       won't be possible until our interpreter understands function types as
       a list of types -}
    remainder = head (drop (length args) sig)
    partial = any (\x -> case x of
                    Checks _ -> False
                    _        -> True
                  ) args

    failIndex = elemIndex False checks
    failExp =
      case failIndex of
        Just i  -> getAt i sig
        Nothing -> Nothing
    failGot =
      case failIndex of
        Just i ->
          case getAt i outTypes of
            Just (Just t) -> Just t
            _             -> Nothing
        _ -> Nothing


  in
    case remainder of
      Just r  ->
        if all (\a -> a) checks then
          if partial then
            Partial r
          else
            Checks r
        else
          case (failIndex, failExp, failGot) of
            (Just i, Just exp, Just got) ->
              -- arbitrary decision to 1-index args, discuss?
              Fails (i + 1) exp got r
            _ ->
              Invalid

      Nothing -> Invalid


substitute : Term -> Term -> String -> Term
substitute old new n =
  case old of
    CTerm c -> CTerm c
    VTerm v -> if n == v then new else VTerm v
    Lam t1 t2 -> if t1 == n then Lam t1 t2 else Lam t1 (substitute t2 new n)
    App t1 t2 -> App (substitute t1 new n) (substitute t2 new n)
    Plus t1 t2 -> Plus (substitute t1 new n) (substitute t2 new n)
    Minus t1 t2 -> Minus (substitute t1 new n) (substitute t2 new n)
    Times t1 t2 -> Times (substitute t1 new n) (substitute t2 new n)
    Eq t1 t2 -> Eq (substitute t1 new n) (substitute t2 new n)
    And t1 t2 -> And (substitute t1 new n) (substitute t2 new n)
    Or t1 t2 -> Or (substitute t1 new n) (substitute t2 new n)
    any -> any


typeCheckApp : Env -> Term -> CheckResult
typeCheckApp env t =
  case t of
    App x y ->
      case x of
        Lam w z -> typecheck env (substitute z y w)
        VTerm v ->
          let lambda = lookup env v
            in case lambda of
              Just (Lam w z) -> typecheck env (substitute z y w)
              _ -> Invalid
        _ -> Invalid
    _ -> Invalid


getTypeList : Env -> Term -> List VType
getTypeList env t =
  case t of
    CTerm c ->
      case c of
        CBool _ -> [TBool]
        CInt _  -> [TInt]

    VTerm v ->
      case lookup env v of
        Just sub -> []
        Nothing -> []

    Plus _ _  -> [TInt, TInt, TInt]
    Minus _ _ -> [TInt, TInt, TInt]
    Times _ _ -> [TInt, TInt, TInt]
    Eq _ _    -> [TInt, TInt, TBool]
    And _ _   -> [TBool, TBool, TBool]
    Or _ _    -> [TBool, TBool, TBool]
    _         -> []


argPosition : String -> Term -> Int
argPosition a b =
  let z = VTerm a in
    case b of
      Plus x y ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      Minus x y ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      Times x y ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      Eq x y    ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      And x y   ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      Or x y    ->
        if x == z && y == z then 3
        else if x == z && y /= z then 2
        else if x /= z && y == z then 1
        else 0
      _ -> -1

typecheck : Env -> Term -> CheckResult
typecheck env t =
  let
    -- curry environment into the typechecker right away
    check = typecheck env
    sig = getTypeList env t

    args =
      case t of
        CTerm _   -> []
        VTerm _   -> []
        Plus x y  -> [check x, check y]
        Minus x y -> [check x, check y]
        Times x y -> [check x, check y]
        Eq x y    -> [check x, check y]
        And x y   -> [check x, check y]
        Or x y    -> [check x, check y]
        _         -> []
  in
    case t of
      VTerm v ->
        case lookup env v of
          Just sub -> check sub
          Nothing  -> Invalid

      Lam a b ->
        let
          newEnv =
            case b of
              Plus x y -> extend env (a, CTerm (CInt 0), TInt)
              Minus x y -> extend env (a, CTerm (CInt 0), TInt)
              Times x y -> extend env (a, CTerm (CInt 1), TInt)
              Eq x y -> extend env (a, CTerm (CInt 0), TInt)
              And x y -> extend env (a, CTerm (CBool False), TBool)
              Or x y -> extend env (a, CTerm (CBool True), TBool)
              _ -> extend env (a, Missing, TNone)
          typeSig = getTypeList newEnv b
        in
          case typeSig of
            [m] ->
              case typecheck newEnv b of
                Checks y -> Checks (TFun TAny y)
                _ -> Invalid

            [m, n, p] ->
              let it =
                    case argPosition a b of
                      3 -> m
                      2 -> n
                      1 -> m
                      0 -> TAny
                      _ -> TAny -- should not be hit
              in
                case typecheck newEnv b of
                  Checks ot -> Checks (TFun it ot)
                  Partial ot -> Partial (TFun it ot)
                  Fails i ot1 ot2 ot3 -> Fails i (TFun it ot1) (TFun it ot2) (TFun it ot3)
                  Invalid -> Invalid

            _ -> Invalid
      App x y -> typeCheckApp env t
      _ -> checkSig sig args

getTypeArgs : Env -> Term -> List CheckResult
getTypeArgs env t =
  let check = typecheck2 env in
    case t of
      CTerm _   -> []
      VTerm _   -> []
      Plus x y  -> (check x) :: (check y) :: []
      Minus x y -> (check x) :: (check y) :: []
      Times x y -> (check x) :: (check y) :: []
      Eq x y    -> (check x) :: (check y) :: []
      And x y   -> (check x) :: (check y) :: []
      Or x y    -> (check x) :: (check y) :: []
      _         -> []

checkSig2 : List VType -> List CheckResult -> CheckResult
checkSig2 sig args =
  let
    outTypes =
      map
        (\x ->
          case x of
            Checks t      -> Just t
            Fails _ _ _ t -> Just t
            Partial t     -> Just t
            Invalid       -> Nothing
        )
        args

    checks = map2 (\x y ->
                    case y of
                      Just y2  -> x == y2
                      Nothing -> True
                  ) sig outTypes
    {- this line is not correct! it should be `drop (length args) sig`, but
       won't be possible until our interpreter understands function types as
       a list of types -}
    remainder = head (drop (length args) sig)
    partial = any (\x -> case x of
                    Checks _ -> False
                    _        -> True
                  ) args

    failIndex = elemIndex False checks
    failExp =
      case failIndex of
        Just i  -> getAt i sig
        Nothing -> Nothing
    failGot =
      case failIndex of
        Just i ->
          case getAt i outTypes of
            Just (Just t) -> Just t
            _             -> Nothing
        _ -> Nothing


  in
    case remainder of
      Just r  ->
        if all (\a -> a) checks then
          if partial then
            Partial r
          else
            Checks r
        else
          case (failIndex, failExp, failGot) of
            (Just i, Just exp, Just got) ->
              -- arbitrary decision to 1-index args, discuss?
              Fails (i + 1) exp got r
            _ ->
              Invalid

      Nothing -> Invalid

getTypeList2 : VType -> List VType
getTypeList2 vt =
  case vt of
    TBool -> [TBool]
    TInt -> [TInt]
    TAny -> [TAny]
    TNone -> [TNone]
    TFun vt1 vt2 -> (getTypeList2 vt1) ++ (getTypeList2 vt2)

generateTypeList : Env -> Term -> List VType
generateTypeList env t =
  case t of
    VTerm s ->
      case lookup env s of
        Just term -> [getTypeSignature term] --assuming it already exists
        Nothing -> []
    Plus x y ->
      case (generateTypeList env x, generateTypeList env y) of
        ([], []) -> []
        (a, []) -> a
        ([], b) -> b
        (a, b) ->
          case (x, y) of
            (VTerm m, VTerm n) ->
              if m == n then a else a ++ b
            _ -> a
    Lam a b -> generateTypeList (insertArg env a b) b
    _ -> []

modifyArgs : Env -> Term -> Env
modifyArgs env t =
  case t of
    Lam a b ->
      case getTypeSignature b of
        TInt -> modifyArgs (replaceVar env (a,CTerm (CInt 0), TInt)) b
        TBool -> modifyArgs (replaceVar env (a,CTerm (CBool True), TBool)) b
        TFun x y ->
          case y of
            TInt -> modifyArgs (replaceVar env (a,CTerm (CInt 0), TInt)) b
            TBool -> modifyArgs (replaceVar env (a,CTerm (CBool True), TBool)) b
            _ -> modifyArgs env b
        TAny -> modifyArgs env b
        TNone -> modifyArgs env b
    _ -> env

insertArgs : Env -> Term -> Env
insertArgs env t =
  case t of
    Lam a b -> insertArgs (extend env (a, EmptyTree, TNone)) b
    _ -> env

insertArg : Env -> String -> Term -> Env
insertArg env a b =
  case b of
    Plus x y -> extend env (a, CTerm (CInt 0), TInt)
    Minus x y -> extend env (a, CTerm (CInt 0), TInt)
    Times x y -> extend env (a, CTerm (CInt 1), TInt)
    Eq x y -> extend env (a, CTerm (CInt 0), TInt)
    And x y -> extend env (a, CTerm (CBool False), TBool)
    Or x y -> extend env (a, CTerm (CBool True), TBool)
    Lam x y ->
      let newEnv = insertArg env x y in
        case typecheck2 newEnv y of --temporarily
          Checks z ->
            case z of
              TInt -> extend newEnv (a, CTerm (CInt 0), TInt)
              TBool -> extend newEnv (a, CTerm (CBool True), TBool)
              _ -> extend newEnv (a, CTerm (CBool False), TAny) -- Change later
          Partial z -> --It's ok if it's partial.. I think
            case z of
              TInt -> extend newEnv (a, CTerm (CInt 0), TInt)
              TBool -> extend newEnv (a, CTerm (CBool True), TBool)
              _ -> extend newEnv (a, CTerm (CBool False), TAny) -- Change later
          _ -> extend newEnv (a, CTerm (CBool False), TNone) -- Change
    _ -> extend env (a, Missing, TNone) -- Change

typecheck2 : Env -> Term -> CheckResult
typecheck2 env t =
  let
    -- curry environment into the typechecker right away
    check = typecheck2 env
    sig = getTypeList2 (getTypeSignature t)
    args = getTypeArgs env t
  in
    case t of
      VTerm v ->
        case lookup env v of
          Just sub -> check sub
          Nothing  -> Invalid

      Lam a b ->
        -- insertArgs env (Lam a b)
        let
          newEnv =  modifyArgs (insertArgs env (Lam a b)) (Lam a b)
          funcSignList =
            case check b of
              Checks m -> (generateTypeList newEnv b) ++ [m]
              Partial m -> (generateTypeList newEnv b) ++ [m]
              _ -> (generateTypeList newEnv b) ++ [TInt]
              -- _ -> [TBool] --Change later
          funcSign = listToTypeSign funcSignList
        in
          case lookupName newEnv t of
            Just s ->
              case lookupType newEnv s of
                Just vt ->
                  let
                    funcArgs = map (\x -> Checks x) (getTypeList2 vt)
                    outType = checkSig2 funcSignList funcArgs
                  in
                    case (length funcSignList) == (length funcArgs) of
                      True -> Checks funcSign
                      False -> Invalid
                    -- case outType of
                    --   Checks _ -> Checks funcSign
                    --   Partial _ -> Partial funcSign --Change later
                    --   Fails i t1 t2 t3-> Fails i t1 t2 t3 --Change later
                    --   Invalid -> Checks funcSign

                Nothing -> Invalid
            Nothing -> Invalid

      App x y -> typeCheckApp env t
      _ -> checkSig sig args
