module Main (..) where

import Html exposing (..)
import StartApp.Simple as StartApp
import MatchList

main: Signal Html
main =
  StartApp.start
    { model = MatchList.initialModel
    , view = MatchList.view
    , update = MatchList.update
    }
