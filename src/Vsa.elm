module Vsa exposing
  ( Vsa
  , empty
  , merge
  , fromExp
  , debugString
  )

import Exp exposing (Exp(..))

import Utils

type alias Vsa =
  List VsaTree

type VsaTree
  = Var (List String)
  | Num (List Int)
  | Plus Vsa Vsa
  | OperandHole
  | OperatorHole Vsa Vsa

empty : Vsa
empty =
  []

mergeTree : VsaTree -> VsaTree -> Vsa
mergeTree t1 t2 =
  case (t1, t2) of
    (Var xs1, Var xs2) ->
      [Var (Utils.deduplicate (xs1 ++ xs2))]

    (Num ns1, Num ns2) ->
      [Num (Utils.deduplicate (ns1 ++ ns2))]

    (Plus vsa11 vsa12, Plus vsa21 vsa22) ->
      [Plus (merge vsa11 vsa21) (merge vsa12 vsa22)]

    (OperandHole, OperandHole) ->
      [OperandHole]

    (OperatorHole vsa11 vsa12, OperatorHole vsa21 vsa22) ->
      [OperatorHole (merge vsa11 vsa21) (merge vsa12 vsa22)]

    _ ->
      [t1, t2]

-- TODO: definitely not correct! consider a+b merged with c+d
merge : Vsa -> Vsa -> Vsa
merge vts1 vts2 =
  List.concatMap
    (\vt1 -> List.concatMap (mergeTree vt1) vts2)
    vts1

fromExp : Exp -> Vsa
fromExp e =
  case e of
    Exp.Var x ->
      [Var [x]]

    Exp.Num n ->
      [Num [n]]

    Exp.Plus e1 e2 ->
      [Plus (fromExp e1) (fromExp e2)]

    Exp.OperandHole ->
      [OperandHole]

    Exp.OperatorHole e1 e2 ->
      [OperatorHole (fromExp e1) (fromExp e2)]

vsaTreeDebugString : VsaTree -> String
vsaTreeDebugString vsaTree =
  case vsaTree of
    Var xs ->
      String.join ", " xs

    Num ns ->
      String.join ", " (List.map String.fromInt ns)

    Plus vsa1 vsa2 ->
      "(+ "
        ++ debugString vsa1
        ++ " "
        ++ debugString vsa2
        ++ ")"

    OperandHole ->
      "?"

    OperatorHole vsa1 vsa2 ->
      "(?op "
        ++ debugString vsa1
        ++ " "
        ++ debugString vsa2
        ++ ")"

debugString : Vsa -> String
debugString vsa =
  "{" ++ String.join ", " (List.map vsaTreeDebugString vsa) ++ "}"
