module ParseForest exposing
  ( ParseForest(..)
  , fromList
  , debug
  )

import Exp exposing (Exp(..))

import Utils

-- type ParseForest
--   = Or (List ParseForest)
--   | Var String
--   | Num Int
--   | Plus ParseForest ParseForest
--   | OperandHole
--   | OperatorHole ParseForest ParseForest

type ParseForest =
  P (List Exp)

fromList : List Exp -> ParseForest
fromList exps =
  P (Utils.deduplicate exps)

debug : ParseForest -> String
debug (P exps) =
  "{" ++ String.join ", " (List.map Exp.debug exps) ++ "}"
