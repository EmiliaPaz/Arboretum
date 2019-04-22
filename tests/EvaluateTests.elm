module EvaluateTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Evaluate exposing (eval, Val(..))
import Environment exposing (Env)
import Types exposing (..)

suite : Test
suite =
  describe "The Evaluate module"
    [ describe "Evaluate.eval" -- Nest as many descriptions as you like.
      [ test "const int evaluates to int" <|
        \_ ->
          let
            env = []
          in
            CTerm (CInt 42)
              |> eval env
              |> Expect.equal (Just (VInt 42))

      , test "const bool checks to bool" <|
        \_ ->
          let
            env = []
          in
            CTerm (CBool False)
              |> eval env
              |> Expect.equal (Just (VBool False))

      , test "adding an int and bool fails" <|
        \_ ->
          let
            env = []
          in
            Plus (CTerm (CBool False)) (CTerm (CInt 7))
              |> eval env
              |> Expect.equal Nothing

      , test "addition" <|
        \_ ->
          let
            env = []
          in
            Plus (CTerm (CInt 5)) (CTerm (CInt 3))
              |> eval env
              |> Expect.equal (Just (VInt 8))
      
      , test "or" <|
        \_ ->
          let
            env = []
          in
            Or (CTerm (CBool True)) (CTerm (CBool False))
              |> eval env
              |> Expect.equal (Just (VBool True))

      , test "and" <|
        \_ ->
          let
            env = []
          in
            And (CTerm (CBool True)) (CTerm (CBool False))
              |> eval env
              |> Expect.equal (Just (VBool False))
      
      , test "if with true evaluates first branch" <|
        \_ ->
          let
            env = []
          in
            If (CTerm (CBool True)) (CTerm (CInt 42)) (CTerm (CInt 62))
              |> eval env
              |> Expect.equal (Just (VInt 42))

      , test "if with false evaluates second branch" <|
        \_ ->
          let
            env = []
          in
            If (CTerm (CBool False)) (CTerm (CInt 42)) (CTerm (CInt 62))
              |> eval env
              |> Expect.equal (Just (VInt 62))

      , test "if with an integer doesn't evaluate" <|
        \_ ->
          let
            env = []
          in
            If (CTerm (CInt 3)) (CTerm (CInt 42)) (CTerm (CInt 62))
              |> eval env
              |> Expect.equal Nothing

      , test "variable substitution" <|
        \_ ->
          let
            env = [("x", CTerm (CInt 9), TInt)]
          in
            VTerm "x"
              |> eval env
              |> Expect.equal (Just (VInt 9))
              
       , test "lambdas evaluate to closures" <|
        \_ ->
          let
            env = []
          in
            Lam "x" (Plus (VTerm "x") (CTerm (CInt 2)))
              |> eval env
              |> Expect.equal (Just (VFun env "x" (Plus (VTerm "x") (CTerm (CInt 2)))))
              
        , test "applications evaluate with environment" <|
        \_ ->
          let
            env = []
          in
            App (Lam "x" (Plus (VTerm "x") (CTerm (CInt 3)))) (CTerm (CInt 5))
              |> eval env
              |> Expect.equal (Just (VInt 8))
      ]
    ]
