module Token exposing
  ( contents
  , shape
  , reverseMany
  , fits
  )

import Lang exposing (..)

contents : Token -> String
contents tok =
  case tok of
    LPAREN ->
      "("

    RPAREN ->
      ")"

    VAR x ->
      x

    NUM n ->
      String.fromInt n

    PLUS ->
      "+"

    OPERAND_HOLE ->
      "?"

    OPERATOR_HOLE ->
      "?"

shape : Token -> Shape
shape tok =
  case tok of
    LPAREN ->
      (Left, Left)

    RPAREN ->
      (Right, Right)

    VAR _ ->
      (Left, Right)

    NUM _ ->
      (Left, Right)

    PLUS ->
      (Right, Left)

    OPERAND_HOLE ->
      (Left, Right)

    OPERATOR_HOLE ->
      (Right, Left)

reverse : Token -> Token
reverse tok =
  case tok of
    LPAREN ->
      RPAREN

    RPAREN ->
      LPAREN

    _ ->
      tok

reverseMany : List Token -> List Token
reverseMany toks =
  case toks of
    [] ->
      []

    head :: tail ->
      reverseMany tail ++ [reverse head]

fits : Token -> Token -> Bool
fits tok1 tok2 =
  Tuple.second (shape tok1) == Tuple.first (shape tok2)
