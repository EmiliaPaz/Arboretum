module TypecheckTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Typecheck exposing (CheckResult(..), VType(..), typecheck)
import Types exposing (..)

-- code repition; this should be refactored out in later commits
lookup : List Var -> String -> Maybe Term
lookup e s =
  case e of
    [] ->
      Nothing

    v :: vs ->
      if v.name == s then
        Just v.term
      else lookup vs s


suite : Test
suite =
  describe "The Typecheck module"
    [ describe "Typecheck.typecheck" -- Nest as many descriptions as you like.
      [ test "const int checks to int type" <|
        \_ ->
          let
            env = lookup []
          in
            CTerm (CInt 0)
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "const bool checks to bool type" <|
        \_ ->
          let
            env = lookup []
          in
            CTerm (CBool False)
              |> typecheck env
              |> Expect.equal (Checks TBool)

      , test "integer addition produces int type" <|
        \_ ->
          let
            env = lookup []
          in
            Plus (CTerm (CInt 21)) (CTerm (CInt 21))
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "boolean operations produces bool type" <|
        \_ ->
          let
            env = lookup []
          in
            Or (CTerm (CBool False)) (CTerm (CBool True))
              |> typecheck env
              |> Expect.equal (Checks TBool)

      , test "variables evaluate to their types" <|
        \_ ->
          let
            env = lookup [{name="a",term=CTerm(CInt 0)}]
          in
            VTerm "a"
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "invalid 1st arg produces failure" <|
        \_ ->
          let
            env = lookup []
          in
            Plus (CTerm (CBool False)) (CTerm (CInt 2))
              |> typecheck env
              |> Expect.equal (Fails 1 TInt TBool TInt)

      , test "invalid 2nd arg produces failure" <|
        \_ ->
          let
            env = lookup []
          in
            Plus (CTerm (CInt 2)) (CTerm (CBool False))
              |> typecheck env
              |> Expect.equal (Fails 2 TInt TBool TInt)

      , test "partial type is output when bad types are further upstream" <|
        \_ ->
          let
            env = lookup []
          in
            Plus (CTerm (CInt 2)) (Plus (CTerm (CBool False)) (CTerm (CInt 3)))
              |> typecheck env
              |> Expect.equal (Partial TInt)

      , test "non-existant varaible produces invalid" <|
        \_ ->
          let
            env = lookup []
          in
            VTerm "a"
              |> typecheck env
              |> Expect.equal (Invalid)
      ]
    ]