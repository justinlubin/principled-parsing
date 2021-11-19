module Shape exposing
  ( Shape
  , Side(..)
  , invertSide
  )

type Side
  = Left
  | Right

type alias Shape =
  (Side, Side)

invertSide : Side -> Side
invertSide s =
  case s of
    Left ->
      Right

    Right ->
      Left
