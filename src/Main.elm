module Main exposing (..)

import Browser

import Model exposing (Model)
import Update exposing (Msg)
import View

main : Program () Model Msg
main =
  Browser.sandbox
    { init = Model.init
    , update = Update.update
    , view = View.view
    }
