module Update exposing
  ( Msg(..)
  , update
  )

import Model exposing (Model)

type Msg =
  InputChanged String

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputChanged newInput ->
      { model | input = newInput }
