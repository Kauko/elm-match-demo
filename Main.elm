module Main (..) where

import Html exposing (..)
import StartApp.Simple as StartApp
import MatchList
import TeamList
import Signal exposing (..)


main : Signal Html
main =
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }


type alias Model =
  { teamListModel : TeamList.Model
  , matchListModel : MatchList.Model
  }


initialModel : { matchListModel : MatchList.Model, teamListModel : TeamList.Model }
initialModel =
  { matchListModel =
      MatchList.createMatches
        MatchList.initialModel
        [ ( "Colts", "Patriots" ), ( "Colts", "Jaguars" ) ]
  , teamListModel =
      TeamList.createTeams
        TeamList.initialModel
        [ ( "Jacksonville", "Jaguars" ), ( "Indianapolis", "Colts" ) ]
  }


type Action
  = MatchUpdate MatchList.Action
  | TeamUpdate TeamList.Action


update : Action -> Model -> Model
update action model =
  case action of
    MatchUpdate a ->
      { model | matchListModel = MatchList.update a model.matchListModel }

    TeamUpdate a ->
      { model | teamListModel = TeamList.update a model.teamListModel }


view : Address Action -> Model -> Html
view address model =
  let
    teamAddress =
      (Signal.forwardTo address TeamUpdate)
  in
    div
      []
      [ TeamList.view teamAddress model.teamListModel
      , MatchList.view
          (Signal.forwardTo address MatchUpdate)
          teamAddress
          model.matchListModel
      ]
