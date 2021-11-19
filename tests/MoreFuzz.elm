module MoreFuzz exposing
  ( token
  , exp
  )

import Main exposing (..)
import Fuzz exposing (..)

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
