module Check exposing
  ( shape
  , balance
  )

import Shape
import Token exposing (Token(..))
import Exp exposing (Exp(..))

innerShape : List Token -> Bool
innerShape toks =
  case toks of
    [] ->
      True

    [_] ->
      True

    first :: second :: rest ->
      Token.fits first second && innerShape (second :: rest)

outerShape : List Token -> Bool
outerShape toks =
  case toks of
    [] ->
      True

    head :: tail ->
      let
        last =
          tail
            |> List.reverse
            |> List.head
            |> Maybe.withDefault head
      in
      Tuple.first (Token.shape head) == Shape.Left
        && Tuple.second (Token.shape last) == Shape.Right

shape : List Token -> Bool
shape toks =
  innerShape toks && outerShape toks

balance : List Token -> Bool
balance =
  let
    helper : Int -> List Token -> Bool
    helper depth toks =
      case toks of
        [] ->
          depth == 0

        LPAREN :: tail ->
          helper (depth + 1) tail

        RPAREN :: tail ->
          helper (depth - 1) tail

        _ :: tail ->
          helper depth tail
  in
  helper 0
