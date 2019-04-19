module Environment exposing (Env, lookup, lookupType, lookupName, extend, varsToEnv, replaceType, replaceTerm, envToVars, addOrModify)

import List exposing (map)
import Types exposing (Term(..), Var, VType(..))
type alias Env = List (String, Term, VType)

{-
  Note: It might be useful combine lookup and lookupType into a single function.
-}

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
  This is a bit happy. There are shadowing issues when the user inputs identical
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
    Nothing -> extend e (s,t,TNone) --Can't replace


{-
  If the variable is part of the environment, replace its type.
  Otherwise, insert the variable into the environment (with unknown term).
-}
replaceType : Env -> String -> VType -> Env
replaceType e s vt =
  case lookup e s of
    Just t -> extend (List.filter (doesNotMatch s) e) (s, t, vt)
    Nothing -> extend e (s,EmptyTree,vt) --Can't replace

{-
  If the variable is part of the environment and either the term of the type
  is unknown, replace the unknown term/variable with a specific term/variable.
  Otherwise, insert the variable into the environment.
-}
addOrModify : Env -> (String, Term, VType) -> Env
addOrModify e (s, t, vt) =
  case (lookup e s, lookupType e s) of
    (Just t1, Just vt1) ->
      case (t1, vt1) of
        (EmptyTree, TNone) -> extend (List.filter (doesNotMatch s) e) (s, t, vt)
        (EmptyTree, vt2) -> extend (List.filter (doesNotMatch s) e) (s, t, vt2)
        (t2, TNone) -> extend (List.filter (doesNotMatch s) e) (s, t2, vt)
        (_, _) -> extend (List.filter (doesNotMatch s) e) (s,t,vt)
    (Just _, Nothing) -> extend (List.filter (doesNotMatch s) e) (s,t,vt) --SHOULDNT HAPPEN
    (Nothing, Just _) -> extend (List.filter (doesNotMatch s) e) (s,t,vt) --SHOULDNT HAPPEN
    (Nothing, Nothing) -> extend e (s,t,vt) --Can't replace

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
