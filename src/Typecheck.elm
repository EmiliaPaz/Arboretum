module Typecheck exposing (CheckResult(..), typeToString, checkResultToString, typecheck)
import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (Const(..), Term(..), VType(..), getTypeSignature, listToTypeSign, typeSignToList)
import Environment exposing (Env, lookup, lookupType, lookupName, extend, addOrModify)



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

{-
  Returns the types of the arguments to the binary operations for type checking.
-}
getTypeArgs : Env -> Term -> List CheckResult
getTypeArgs env t =
  let check = typecheck env in
    case t of
      Plus x y  -> (check x) :: (check y) :: []
      Minus x y -> (check x) :: (check y) :: []
      Times x y -> (check x) :: (check y) :: []
      Eq x y    -> (check x) :: (check y) :: []
      And x y   -> (check x) :: (check y) :: []
      Or x y    -> (check x) :: (check y) :: []
      Tuple x y -> (check x) :: (check y) :: []
      _         -> []

{-
  Inserts a lambda's argument into the environment (takes into account nested lambdas).
-}
insertArgs : Env -> Term -> Env
insertArgs env t =
  case t of
    Lam a b -> insertArgs (extend env (a, EmptyTree, TInt)) b --Defaults to int
    _ -> env

{-
  Uses checkSig for everything except VTerm, Lam, and App.
-}
typecheck : Env -> Term -> CheckResult
typecheck env t =
  let
    -- curry environment into the typechecker right away
    check = typecheck env
    sig =
      case getTypeSignature t of
        Just vt -> typeSignToList vt
        Nothing -> []
    args = getTypeArgs env t
  in
    case t of
      VTerm v ->
        case lookup env v of
          Just sub -> check sub
          Nothing  -> Invalid

      Lam a b ->
        typecheckLam env t

      App x y -> typecheckApp env t

      Tuple m n ->
        case (typecheck env m, typecheck env n) of
          -- Checks a
          (Checks a, Checks x)            -> Checks (TTuple a x)
          (Checks a, Fails w x y z)       -> Fails 2 (TTuple a x) (TTuple a y) (TTuple a z)
          (Checks a, Partial x)           -> Partial (TTuple a x)
          -- Fails a
          (Fails a b c d, Checks x)       -> Fails 1 (TTuple b x) (TTuple c x) (TTuple d x)
          (Fails a b c d, Fails w x y z)  -> Fails 1 (TTuple b x) (TTuple c y) (TTuple d z)  -- should put on red both (c,y)
          (Fails a b c d, Partial x)      -> Fails 1 (TTuple b x) (TTuple c x) (TTuple d x)
          -- Partial a
          (Partial a, Checks x)           -> Partial (TTuple a x)
          (Partial a, Fails w x y z)      -> Fails 2 (TTuple a x) (TTuple a y) (TTuple a z)
          (Partial a, Partial x)           -> Partial (TTuple a x)
          -- Invalid a or x
          (Invalid, _)                    -> Invalid
          (_, Invalid)                    -> Invalid
          
      _ -> checkSig sig args



{-
  Insert the arguments into the environment, look up the annotated type of this function,
  and assume this type is correct. This is a temporary feature. More work needs to be done
  here and it will probably imply implementing type inference.
-}
typecheckLam : Env -> Term -> CheckResult
typecheckLam env t =
  case t of
    Lam a b ->
      let
        newEnv =  insertArgs env (Lam a b)
      in
        case lookupName newEnv t of --We might want to find a different way of doing this (see comments for lookupName in Environment)
          Just s ->
            case lookupType newEnv s of
              Just vt -> Checks vt --For now, the user's type annotation is assumed to be correct.
              Nothing -> Invalid
          Nothing -> Invalid
    _ -> Invalid -- Shouldn't be reached


{-
  This function works the same as before except we use the environment directly rather than using substitution.
-}
typecheckApp : Env -> Term -> CheckResult
typecheckApp env t =
  case t of
    App x y ->
      case x of
        Lam w z ->
          case getTypeSignature y of
            Just vt ->
              let newEnv = addOrModify env (True, True) (w, y, vt) in
                typecheck newEnv z
            Nothing -> Invalid
        VTerm v ->
          let lambda = lookup env v
            in case lambda of
              Just (Lam w z) ->
                case getTypeSignature y of
                  Just vt ->
                    let newEnv = addOrModify env (True, True) (w, y, vt) in
                      typecheck newEnv z
                  Nothing -> Invalid
              _ -> Invalid
        _ -> Invalid
    _ -> Invalid --Shouldn't be reached
