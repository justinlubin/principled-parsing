module Side exposing
  ( invert
  )

invert : Side -> Side
invert s =
  case s of
    Left ->
      Right

    Right ->
      Left
