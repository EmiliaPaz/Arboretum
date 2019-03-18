module Render exposing (..)

import List exposing (..)
import List.Extra exposing (elemIndex, getAt)
import Types exposing (..)

-------------------------------------- Rendering --------------------------------------
boolToString : Bool -> String
boolToString b = 
  case b of 
    True -> "True"
    False -> "False"


termToString : Term -> String
termToString t =
  case t of
    CTerm x ->
      case x of
        CBool a -> boolToString a
        CInt a -> String.fromInt a
      
    VTerm x ->
      x
    
    Plus t1 t2 ->
      "(" ++ (termToString t1) ++ " + " ++ (termToString t2) ++ ")"

    Minus t1 t2 ->
      "(" ++ (termToString t1) ++ " - " ++ (termToString t2) ++ ")"

    Times t1 t2 ->
      "(" ++ (termToString t1) ++ " * " ++ (termToString t2) ++ ")"

    Eq t1 t2 ->
      "(" ++ (termToString t1) ++ " == " ++ (termToString t2) ++ ")"
    
    And t1 t2 ->
      "(" ++ (termToString t1) ++ " && " ++ (termToString t2) ++ ")"

    Or t1 t2 ->
      "(" ++ (termToString t1) ++ " || " ++ (termToString t2) ++ ")"

    _ -> ""


typeToString : VType -> String
typeToString t =
  case t of
    TBool -> "Bool"
    TInt  -> "Int"


valToString : Maybe Val -> String
valToString v =
  case v of
    Just (VBool x) -> boolToString x
    Just (VInt x)  -> String.fromInt x
    Nothing        -> "Undefined"


-- returns input if input is Just VType, Nothing otherwise
filterTypes : VType -> List (Maybe VType) -> Maybe VType
filterTypes t xs = 
  case xs of
    [] ->
      Nothing

    [x] ->
      if x == (Just t) then
        x
      else
        Nothing

    x :: rest ->
      if x == (Just t) then
        filterTypes t rest
      else
        Nothing


filterInts = filterTypes TInt
filterBools = filterTypes TBool


-- returns Just VType if the term typechecks, Nothing otherwise
typecheck : Env -> Term -> Maybe VType
typecheck e t =
  case t of
    CTerm c ->
      case c of
        CBool _ -> Just TBool
        CInt _ -> Just TInt
    
    VTerm v -> 
      case e v of
        Just subst -> typecheck e subst
        Nothing    -> Nothing
    
    -- binary int operators all have the same behavior
    Plus x y  -> filterInts (map (typecheck e) [x, y])
    Minus x y -> filterInts (map (typecheck e) [x, y])
    Times x y -> filterInts (map (typecheck e) [x, y])

    Eq x y ->
      if filterInts(map (typecheck e) [x, y]) == Just TInt then
        Just TBool
      else if filterBools(map (typecheck e) [x, y]) == Just TBool then
        Just TBool
      else
        Nothing
    
    And x y -> filterBools (map (typecheck e) [x, y])
    Or x y -> filterBools (map (typecheck e) [x, y])
    _ -> Nothing


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


last : List a -> Maybe a
last l =
  case l of
    []      -> Nothing
    [x]     -> Just x
    x :: xs -> last xs


-- checks a function signature `sig` against a list of argument types `args`
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
              Fails i exp got r
            _ ->
              Invalid

      Nothing -> Invalid


{-
checkSig : Int -> List VType -> List CheckResult -> CheckResult
checkSig argNum sig args =
  let
    checkNext = checkSig (argNum + 1)
  in
  case sig of
    []        -> Invalid

    s :: rsig ->
      let
        final =
          case last rsig of
            Just lst -> lst
            Nothing  -> s

      in
        case args of
          [] ->
            case rsig of
              [] -> Checks s
              {- while we have no currying this returns Nothing, but eventually it
                should return the curried type -}
              _   -> Invalid
          
          a :: rargs ->
            case a of
              Checks t ->
                case t == s of
                  True  -> checkNext rsig rargs
                  False -> Fails argNum s t final
              
              Fails _ exp got out ->
                case out == s of
                  True  ->
                    case checkNext rsig rargs of
                      Checks t2 -> Partial t2
                      _        -> checkNext rsig rargs
                  False -> Fails argNum s out final

              Partial t ->
                case t == s of
                  True  ->
                    case checkNext rsig rargs of
                      Checks t2 -> Partial t2
                      _        -> checkNext rsig rargs
                  False -> Fails argNum s t final
              
              Invalid ->
                case checkNext rsig rargs of
                  Checks t2 -> Partial t2
                  _         -> checkNext rsig rargs
-}


typecheck3 : Env -> Term -> CheckResult
typecheck3 env t =
  let
    -- curry environment into the typechecker right away
    check = typecheck3 env
    sig =
      case t of
        CTerm c ->
          case c of
            CBool _ -> [TBool]
            CInt _  -> [TInt]
        
        VTerm v ->
          case env v of
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
        case env v of
          Just sub -> typecheck3 env sub
          Nothing  -> Invalid
      
      _ -> checkSig2 sig args


tryBinFn : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
tryBinFn f mx my = 
  case mx of
    Just x ->
      case my of
        Just y  -> Just (f x y)
        Nothing -> Nothing
    
    Nothing -> Nothing

tryBool : Maybe Val -> Maybe Bool
tryBool mx =
  case mx of
    Just (VBool x) -> Just x
    _              -> Nothing

tryInt : Maybe Val -> Maybe Int
tryInt mx =
  case mx of
    Just (VInt x) ->  Just x
    _              -> Nothing

wrapInt : Maybe Int -> Maybe Val
wrapInt c =
  case c of
    Just x  -> Just (VInt x)
    Nothing -> Nothing

wrapBool : Maybe Bool -> Maybe Val
wrapBool c =
  case c of
    Just x  -> Just (VBool x)
    Nothing -> Nothing

-- an approximaton of an 'or' operation with maybe
takeOne : (Maybe a, Maybe a) -> Maybe a
takeOne (mx, my) =
  case mx of
    Just x  -> Just x
    Nothing ->
      case my of
        Just y ->  Just y
        Nothing -> Nothing


-- evaluates a term
eval : Env -> Term -> Maybe Val
eval e t =
  let
    evale = eval e
  in
  case t of
    CTerm c ->
      case c of
        CInt x  -> Just (VInt x)
        CBool x -> Just (VBool x)
    
    VTerm v ->
      case e v of
        Just subst -> evale subst
        Nothing    -> Nothing
    
    Plus x y -> 
      wrapInt ( tryBinFn (+) (tryInt (evale x)) (tryInt (evale y)) )

    Minus x y -> 
      wrapInt ( tryBinFn (-) (tryInt (evale x)) (tryInt (evale y)) )

    Times x y -> 
      wrapInt ( tryBinFn (*) (tryInt (evale x)) (tryInt (evale y)) )

    Eq x y ->
      takeOne
        ( wrapBool ( tryBinFn (==) (tryInt (evale x)) (tryInt (evale y)) )
        , wrapBool ( tryBinFn (==) (tryBool (evale x)) (tryBool (evale y)) )
        )

    And x y -> 
      wrapBool ( tryBinFn (&&) (tryBool (evale x)) (tryBool (evale y)) )
    
    Or x y -> 
      wrapBool ( tryBinFn (||) (tryBool (evale x)) (tryBool (evale y)) )

    _ -> Nothing
