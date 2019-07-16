module Environment exposing (TypeEnv, TermEnv)

import Dict exposing (Dict)
import List exposing (map)
import Types exposing (Term(..), Var, VType(..))

type alias TermEnv = Dict String Term
type alias TypeEnv = Dict String VType 

{-type alias Env =
  { terms : TermEnv
  , types : CheckEnv
  , annotations: TypeEnv
  }-}


{-
  Note: It might be useful combine lookup and lookupType into a single function.
-}

{-
lookup : Env -> String -> Maybe Term
lookup e s =
  case e of
    [] ->
      Nothing

    (id, t, vt) :: vs ->
      if id == s then
        Just t
      else lookup vs s

lookupType : Env -> String -> Maybe VType
lookupType e s =
  case e of
    [] ->
      Nothing

    (id, t, vt) :: vs ->
      if id == s then
        Just vt
      else lookupType vs s

{-
  This is a bit hacky. There are shadowing issues when the user inputs identical
  terms with different names; we should probably find a better way to do this.
-}
lookupName : Env -> Term -> Maybe String
lookupName e t1 =
  case e of
    [] ->
      Nothing

    (id, t2, vt) :: vs ->
      if t1 == t2 then
        Just id
      else lookupName vs t1

{-
  Returns true if the string doesn't match the name of the variable.
-}
doesNotMatch : String -> (String, Term, VType) -> Bool
doesNotMatch s (s1,t,v) =
    if s /= s1 then True
    else False

{-
  If the variable is part of the environment, replace its term.
  Otherwise, insert the variable into the environment (with unknown type).
-}
replaceTerm : Env -> String -> Term -> Env
replaceTerm e s t =
  case lookupType e s of
    Just vt -> extend (List.filter (doesNotMatch s) e) (s, t, vt)
    Nothing -> addOrModify e (True, False) (s,t,TInt) --Can't replace


{-
  If the variable is part of the environment, replace its type.
  Otherwise, insert the variable into the environment (with unknown term).
-}
replaceType : Env -> String -> VType -> Env
replaceType e s vt =
  case lookup e s of
    Just t -> extend (List.filter (doesNotMatch s) e) (s, t, vt)
    Nothing -> addOrModify e (False, True) (s, EmptyTree, vt) --Can't replace

{-
  If the variable is part of the environment and, depending on the pair (Bool, Bool)
  modify the term and/or the type. For instance, the pair (True, False) implies
  switching the term for a new one, but keeping the type as is.
  If the variable does not exist, insert it into the environment.
-}
addOrModify : Env -> (Bool, Bool) -> (String, Term, VType) -> Env
addOrModify e flag (s, t, vt) =
  case (lookup e s, lookupType e s) of
    (Just t1, Just vt1) ->
      case flag of
        (True, True) -> extend (List.filter (doesNotMatch s) e) (s, t, vt) --Replace term and type
        (True, False) -> extend (List.filter (doesNotMatch s) e) (s, t, vt1) --Replace term
        (False, True) -> extend (List.filter (doesNotMatch s) e) (s, t1, vt) --Replace type
        (False, False) -> e --Leave as is
    (Just _, Nothing) -> extend (List.filter (doesNotMatch s) e) (s, t, vt) --Shouldn't happen
    (Nothing, Just _) -> extend (List.filter (doesNotMatch s) e) (s, t, vt) --Shouldn't happen
    (Nothing, Nothing) -> extend e (s, t, vt) --Insert into environment

extend : Env -> (String, Term, VType) -> Env
extend e v =
  [v] ++ e

-- we may eventually want to move away from var, but this stopgap works for now
varsToEnv : List Var -> Env
varsToEnv vs =
  map (\v -> (v.name, v.term, v.vtype)) vs

{-
  The reverse of varsToEnv.
-}
envToVars : Env -> List Var
envToVars env =
  case env of
    t :: ts ->
      case t of
        (a, b, c) -> [{name=a,term=b,vtype=c}] ++ envToVars ts
    _ -> []
-}