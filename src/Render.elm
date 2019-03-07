module Render exposing (..)

import List exposing (map)
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

    EmptyTree -> ""


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
    EmptyTree -> Nothing


-- experimental rewrite of typecheck to provide more information

{-type TypeResult = Checks VType | Fails VType VType | None

testTypes : VType -> VType -> TypeResult
testTypes exp got =
  case exp == got of
    True  -> Checks exp
    False -> Fails exp got

typecheck2 : Env -> VType -> Term -> Tree TypeResult
typecheck2 env exp t = 
  let
    check = typecheck2 env
    test  = testTypes exp
  in
    case t of
      CTerm x ->
        case x of
          CBool _ -> Node (test TBool) []
          CInt  _ -> Node (test TInt) []
      
      VTerm x ->
        case env x of
          Just subst -> Node (test exp) [check exp subst]
          Nothing    -> Node (None) []
      
      Plus  x y -> Node (test TInt) [check TInt x, check TInt y]
      Minus x y -> Node (test TInt) [check TInt x, check TInt y]
      Times x y -> Node (test TInt) [check TInt x, check TInt y]
      Eq x y -> Node (test TBool) [check TInt x, check TInt y]
      And x y -> Node (test TBool) [check TBool x, check TBool y]
      Or x y -> Node (test TBool) [check TBool x, check TBool y]
      EmptyTree -> Node (None) []-}


checkResultToString : CheckResult -> String
checkResultToString r =
  case r of
    Checks t ->
      typeToString t

    Fails argNum exp got out ->
      "At argument " ++ String.fromInt argNum ++ " Expected: " ++ typeToString exp ++ ", Got: " ++ typeToString got
    
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
        And _ _   -> [TBool, TBool, TInt]
        Or _ _    -> [TBool, TBool, TBool]
        EmptyTree -> []
    
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
        EmptyTree -> []
  in
    case t of
      VTerm v ->
        case env v of
          Just sub -> typecheck3 env sub
          Nothing  -> Invalid
      
      _ -> checkSig 1 sig args


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

    EmptyTree -> Nothing
