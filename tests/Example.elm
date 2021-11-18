module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, constant, map2)
import Test exposing (..)

import Main exposing (..)

token : Fuzzer Token
token =
  Fuzz.oneOf
    [ constant LPAREN
    , constant RPAREN
    , constant (VAR "x")
    , constant (NUM 1)
    , constant PLUS
    , constant OPERAND_HOLE
    , constant OPERATOR_HOLE
    ]

exp : Int -> Fuzzer Exp
exp n =
  if n <= 0 then
    constant OperandHole
  else
    Fuzz.oneOf
      [ constant (Var "x")
      , constant (Num 1)
      , map2 Plus (exp (n - 1)) (exp (n - 1))
      , constant OperandHole
      , map2 OperatorHole (exp (n - 1)) (exp (n - 1))
      ]

suite : Test
suite =
  describe "principled parsing"
    [ fuzz (exp 5) "unparse-parse" <| \e ->
        e
          |> toTokens
          |> parse
          |> Expect.equal (Just e)
    , fuzz (list token) "shapeRepair repairs" <| \toks ->
        toks
          |> shapeRepair
          |> shapeChecks
          |> Expect.true "Expected the shape-repaired tokens to shape-check"
    , fuzz (list token) "balanceRepair repairs" <| \toks ->
      if List.length toks > 10 then
        Expect.pass
      else
        toks
          |> shapeRepair
          |> balanceRepair
          |> List.all balanceChecks
          |> Expect.true "Expected all balance-repaired tokens to be balanced"
    ]
