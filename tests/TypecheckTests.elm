module TypecheckTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Typecheck exposing (CheckResult(..), typecheck)
import Environment exposing (Env)
import Types exposing (..)

suite : Test
suite =
  describe "The Typecheck module"
    [ describe "Typecheck.typecheck" -- Nest as many descriptions as you like.
      [ test "const int checks to int type" <|
        \_ ->
          let
            env = []
          in
            CTerm (CInt 0)
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "const bool checks to bool type" <|
        \_ ->
          let
            env = []
          in
            CTerm (CBool False)
              |> typecheck env
              |> Expect.equal (Checks TBool)

      , test "integer addition produces int type" <|
        \_ ->
          let
            env =  []
          in
            Plus (CTerm (CInt 21)) (CTerm (CInt 21))
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "boolean operations produces bool type" <|
        \_ ->
          let
            env =  []
          in
            Or (CTerm (CBool False)) (CTerm (CBool True))
              |> typecheck env
              |> Expect.equal (Checks TBool)

      , test "variables evaluate to their types" <|
        \_ ->
          let
            env = [ ( "a", CTerm (CInt 0) , TInt) ]
          in
            VTerm "a"
              |> typecheck env
              |> Expect.equal (Checks TInt)

      , test "invalid 1st arg produces failure" <|
        \_ ->
          let
            env = []
          in
            Plus (CTerm (CBool False)) (CTerm (CInt 2))
              |> typecheck env
              |> Expect.equal (Fails 1 TInt TBool TInt)

      , test "invalid 2nd arg produces failure" <|
        \_ ->
          let
            env = []
          in
            Plus (CTerm (CInt 2)) (CTerm (CBool False))
              |> typecheck env
              |> Expect.equal (Fails 2 TInt TBool TInt)

      , test "partial type is output when bad types are further upstream" <|
        \_ ->
          let
            env = []
          in
            Plus (CTerm (CInt 2)) (Plus (CTerm (CBool False)) (CTerm (CInt 3)))
              |> typecheck env
              |> Expect.equal (Partial TInt)

      , test "non-existant varaible produces invalid" <|
        \_ ->
          let
            env = []
          in
            VTerm "a"
              |> typecheck env
              |> Expect.equal (Invalid)
      ]
    ]