module Check exposing
  ( shape
  , balance
  )

import Lang exposing (..)
import Token

innerShapeChecks : List Token -> Bool
innerShapeChecks toks =
  case toks of
    [] ->
      True

    [_] ->
      True

    first :: second :: rest ->
      Token.fits first second && innerShapeChecks (second :: rest)

outerShapeChecks : List Token -> Bool
outerShapeChecks toks =
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
      Tuple.first (Token.shape head) == Left
        && Tuple.second (Token.shape last) == Right

shape : List Token -> Bool
shape toks =
  innerShapeChecks toks && outerShapeChecks toks

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
