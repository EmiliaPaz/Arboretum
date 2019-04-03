module Typecheck exposing (CheckResult(..), VType(..), typeToString, checkResultToString, typecheck)
import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (Const(..), Term(..))
import Environment exposing (Env, lookup)


-- V(alue)Type is a type that a TreeAssembly term can evaluate to
type VType = TBool | TInt


typeToString : VType -> String
typeToString t =
  case t of
    TBool -> "Bool"
    TInt  -> "Int"

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


typecheck : Env -> Term -> CheckResult
typecheck env t =
  let
    -- curry environment into the typechecker right away
    check = typecheck env
    sig =
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
      
      _ -> checkSig sig args