module Exp exposing
  ( Exp(..)
  , tokens
  , debug
  )

import Token exposing (Token(..))

type Exp
  = Var String
  | Num Int
  | Plus Exp Exp
  | OperandHole
  | OperatorHole Exp Exp

tokens : Exp -> List Token
tokens e =
  case e of
    Var x ->
      [VAR x]

    Num n ->
      [NUM n]

    Plus e1 e2 ->
      [LPAREN] ++ tokens e1 ++ [PLUS] ++ tokens e2 ++ [RPAREN]

    OperandHole ->
      [OPERAND_HOLE]

    OperatorHole e1 e2 ->
      [LPAREN] ++ tokens e1 ++ [OPERATOR_HOLE] ++ tokens e2 ++ [RPAREN]

debug : Exp -> String
debug e =
  case e of
    Var x ->
      x

    Num n ->
      String.fromInt n

    Plus e1 e2 ->
      "(+ " ++ debug e1 ++ " " ++ debug e2 ++ ")"

    OperandHole ->
      "?"

    OperatorHole e1 e2 ->
      "(?op " ++ debug e1 ++ " " ++ debug e2 ++ ")"


