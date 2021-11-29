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
        if List.length toks > 5 then
          Expect.pass
        else
          toks
            |> Repair.shape
            |> Repair.balance
            |> List.all Check.balance
            |> Expect.true "Expected all balance-repaired tokens to be balanced"
    , fuzz (F.list MF.token) "balanceRepair extends" <| \toks ->
        if List.length toks > 4 then
          Expect.pass
        else
          toks
            |> Repair.shape
            |> Repair.balance
            |> List.all (\xs -> List.length xs >= List.length toks)
            |> Expect.true "Expected balanceRepair to extend list"
    , fuzz (F.list MF.token) "balanceRepair actually does something" <| \toks ->
        if List.length toks > 3 || List.length toks == 0 then
          Expect.pass
        else
          toks
            |> Repair.shape
            |> Repair.balance
            |> List.length
            |> (\n -> n >= 1)
            |> Expect.true "Expected balanceRepair to actually do something"
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
    , describe "rightInsertions"
        ( let
            shouldInsert left maybeRight =
              case maybeRight of
                Nothing ->
                  left < 20

                Just right ->
                  left < 20 && 20 < right

            shouldInsert2 left maybeRight =
              case maybeRight of
                Nothing ->
                  False

                Just right ->
                  left < 20 && 20 < right
          in
            [ test "empty" <| \() ->
                Utils.rightInsertions shouldInsert 20 []
                  |> Expect.equal []
            , test "with one (non-matching) element" <| \() ->
                Utils.rightInsertions shouldInsert 20 [23]
                  |> Expect.equal []
            , test "with one (matching) element" <| \() ->
                Utils.rightInsertions shouldInsert 20 [5]
                  |> Expect.equal [[5, 20]]
            , test "more 1" <| \() ->
                Utils.rightInsertions shouldInsert 20 [0, 23, 6, 4, 21]
                  |> Expect.equal [[0, 20, 23, 6, 4, 21], [0, 23, 6, 4, 20, 21]]
            , test "more 2" <| \() ->
                Utils.rightInsertions shouldInsert 20 [0, 23, 6, 4]
                  |> Expect.equal [[0, 20, 23, 6, 4], [0, 23, 6, 4, 20]]
            , test "more 3" <| \() ->
                Utils.rightInsertions shouldInsert2 20 [0, 23, 6, 4]
                  |> Expect.equal [[0, 20, 23, 6, 4]]
            , fuzz (F.list F.int) "item is in outputs" <| \xs ->
                Utils.rightInsertions shouldInsert 20 xs
                  |> List.all (List.member 20)
                  |> Expect.true "Expected item to be in all outputs"
            ]
        )
    , fuzz (F.list F.int) "insertions has length n + k choose k" <| \xs ->
        let
          n =
            List.length xs

          k =
            6
        in
        if n > 10 then
          Expect.pass
        else
          Utils.insertions k (-1) xs
            |> List.length
            |> Expect.equal (Utils.choose (n + k) k)
    , describe "unmatchedRights"
        [ test "1" <| \() ->
            ")("
              |> String.toList
              |> Utils.unmatchedRights '(' ')'
              |> Expect.equal 1
        , test "2" <| \() ->
            ")())()("
              |> String.toList
              |> Utils.unmatchedRights '(' ')'
              |> Expect.equal 2
        , test "1 again" <| \() ->
            ")(((()))()("
              |> String.toList
              |> Utils.unmatchedRights '(' ')'
              |> Expect.equal 1
        , test "4" <| \() ->
            ")))())"
              |> String.toList
              |> Utils.unmatchedRights '(' ')'
              |> Expect.equal 4
        , test "3" <| \() ->
            ")))(())"
              |> String.toList
              |> Utils.unmatchedRights '(' ')'
              |> Expect.equal 3
        ]
        , test "groupBy" <| \() ->
            Utils.groupBy (==) [1, 1, 2, 3, 4, 4]
              |> Expect.equal [[1, 1], [2], [3], [4, 4]]
    ]
