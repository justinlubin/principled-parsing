module VersionSpaceAlgebra exposing
  ( emptyVsa
  , merge
  , fromExp
  , debugString
  )

import Lang exposing (..)

import Utils

emptyVsa : VSA
emptyVsa =
  []

mergeTree : VSATree -> VSATree -> VSA
mergeTree t1 t2 =
  case (t1, t2) of
    (VVar xs1, VVar xs2) ->
      [VVar (Utils.deduplicate (xs1 ++ xs2))]

    (VNum ns1, VNum ns2) ->
      [VNum (Utils.deduplicate (ns1 ++ ns2))]

    (VPlus vsa11 vsa12, VPlus vsa21 vsa22) ->
      [VPlus (merge vsa11 vsa21) (merge vsa12 vsa22)]

    (VOperandHole, VOperandHole) ->
      [VOperandHole]

    (VOperatorHole vsa11 vsa12, VOperatorHole vsa21 vsa22) ->
      [VOperatorHole (merge vsa11 vsa21) (merge vsa12 vsa22)]

    _ ->
      [t1, t2]

-- TODO: definitely not correct! consider a+b merged with c+d
merge : VSA -> VSA -> VSA
merge vts1 vts2 =
  List.concatMap
    (\vt1 -> List.concatMap (mergeTree vt1) vts2)
    vts1

fromExp : Exp -> VSA
fromExp e =
  case e of
    Var x ->
      [VVar [x]]

    Num n ->
      [VNum [n]]

    Plus e1 e2 ->
      [VPlus (fromExp e1) (fromExp e2)]

    OperandHole ->
      [VOperandHole]

    OperatorHole e1 e2 ->
      [VOperatorHole (fromExp e1) (fromExp e2)]

vsaTreeDebugString : VSATree -> String
vsaTreeDebugString vsaTree =
  case vsaTree of
    VVar xs ->
      String.join ", " xs

    VNum ns ->
      String.join ", " (List.map String.fromInt ns)

    VPlus vsa1 vsa2 ->
      "(+ "
        ++ debugString vsa1
        ++ " "
        ++ debugString vsa2
        ++ ")"

    VOperandHole ->
      "?"

    VOperatorHole vsa1 vsa2 ->
      "(?op "
        ++ debugString vsa1
        ++ " "
        ++ debugString vsa2
        ++ ")"

debugString : VSA -> String
debugString vsa =
  "{" ++ String.join ", " (List.map vsaTreeDebugString vsa) ++ "}"



