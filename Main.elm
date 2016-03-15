module Main (..) where

import Html exposing (..)
import StartApp.Simple as StartApp
import Match

main: Signal Html
main =
  StartApp.start
    { model = Match.initialModel
    , view = Match.view
    , update = Match.update
    }
