module Tests exposing (..)

import Expect
import Fuzz as F
import MoreFuzz as MF
import Test exposing (..)

import Exp
import Translation
import Repair
import Check

suite : Test
suite =
  describe "principled parsing"
    [ fuzz (MF.exp 5) "unparse-parse" <| \e ->
        e
          |> Exp.tokens
          |> Translation.parse
          |> Expect.equal (Just e)
    , fuzz (F.list MF.token) "shapeRepair repairs" <| \toks ->
        toks
          |> Repair.shape
          |> Check.shape
          |> Expect.true "Expected the shape-repaired tokens to shape-check"
    , fuzz (F.list MF.token) "balanceRepair repairs" <| \toks ->
        if List.length toks > 10 then
          Expect.pass
        else
          toks
            |> Repair.shape
            |> Repair.balance
            |> List.all Check.balance
            |> Expect.true "Expected all balance-repaired tokens to be balanced"
    ]
