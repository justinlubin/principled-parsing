module Model exposing
  ( Model
  , init
  )

type alias Model =
  { input : String
  }

init : Model
init =
  { input =
      "(1) + ("
  }
