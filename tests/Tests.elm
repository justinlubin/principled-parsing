module Tests exposing (..)

import Expect
import Fuzz as F
import MoreFuzz as MF
import Test exposing (..)

import Exp
import Translation
import Repair
import Check

import Utils

principledParsing : Test
principledParsing =
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

utils : Test
utils =
  describe "utils"
    [ fuzz (F.list F.int) "is subsequence" <| \xs ->
        Utils.subsequence xs (List.intersperse 0 xs ++ List.drop 5 xs)
          |> Expect.true "Expected xs to be subsequence of modidifications"
    , fuzz (F.list F.int) "is not subsequence" <| \xs ->
        Utils.subsequence (xs ++ [0]) xs
          |> Expect.false "Expected modified xs not to be subsequence of xs"
    , fuzz (F.list F.int) "is not subsequence (intersperse)" <| \xs ->
        case (xs, List.maximum xs) of
          (_ :: _ :: _, Just m) ->
            Utils.subsequence (List.intersperse (m + 1) xs) xs
              |> Expect.false
                   "Expected interspersed xs not to be subsequence of xs"

          _ ->
            Expect.pass
    , test "listIterate" <| \() ->
        Utils.listIterate 2 (\x -> [x, 2]) 5
          |> Expect.equal [5, 2, 2, 2]
    ]
