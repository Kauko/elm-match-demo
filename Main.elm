module Main (..) where

import Html exposing (..)
import StartApp.Simple as StartApp
-- import MatchList
import TeamList
import Signal exposing (..)


main : Signal Html
main =
  StartApp.start
    { model = TeamList.initialModel
    , view = TeamList.view
    , update = TeamList.update
    }


{-
type alias Model =
  { teamList : TeamList.Model
  , matchList : MatchList.Model
  }

initialModel =
  {
  matchList = MatchList.initialModel
  ,teamList = TeamList.initialModel
  }


type Action
  = MatchUpdate MatchList.Action
  | TeamUpdate TeamList.Action

update action model =
  case action of
    MatchUpdate a ->
      {model | matchList = MatchList.update a model.matchList.model}
    TeamUpdate a ->
      {model | teamList = TeamList.update a model.teamList.model}

view : Address Action -> Model -> Html
view address model =
  div
    []
    [ TeamList.view (Signal.forwardTo address TeamUpdate) model.teamList
    , MatchList.view (Signal.forwardTo address MatchUpdate) model.matchList
    ]-}
