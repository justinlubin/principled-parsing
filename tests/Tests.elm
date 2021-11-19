module Tests exposing (..)

import Expect
import Fuzz as F
import MoreFuzz as MF
import Test exposing (..)

import Main exposing (..)

suite : Test
suite =
  describe "principled parsing"
    [ fuzz (MF.exp 5) "unparse-parse" <| \e ->
        e
          |> toTokens
          |> parse
          |> Expect.equal (Just e)
    , fuzz (F.list MF.token) "shapeRepair repairs" <| \toks ->
        toks
          |> shapeRepair
          |> shapeChecks
          |> Expect.true "Expected the shape-repaired tokens to shape-check"
    , fuzz (F.list MF.token) "balanceRepair repairs" <| \toks ->
      if List.length toks > 10 then
        Expect.pass
      else
        toks
          |> shapeRepair
          |> balanceRepair
          |> List.all balanceChecks
          |> Expect.true "Expected all balance-repaired tokens to be balanced"
    ]
