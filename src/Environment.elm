module Environment exposing (Env, lookup, extend, varsToEnv)

import List exposing (map)
import Types exposing (Term(..), Var)

type alias Env = List (String, Term)


lookup : Env -> String -> Maybe Term
lookup e s =
  case e of
    [] ->
      Nothing

    (id, t) :: vs ->
      if id == s then
        Just t
      else lookup vs s


extend : Env -> (String, Term) -> Env
extend e v =
  [v] ++ e

-- we may eventually want to move away from var, but this stopgap works for now
varsToEnv : List Var -> Env
varsToEnv vs =
  map (\v -> (v.name, v.term)) vs