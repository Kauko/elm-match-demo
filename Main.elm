module Main (..) where

import Html exposing (..)
import StartApp as StartApp
import Effects
import Task
import MatchList
import TeamList
import Signal exposing (..)


app : StartApp.App { matchListModel : MatchList.Model, teamListModel : TeamList.Model }
app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , inputs = []
    , view = view
    , update = update
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


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


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    MatchUpdate a ->
      let
        ( newModel, newEffects ) =
          MatchList.update a model.matchListModel
      in
        ( { model | matchListModel = newModel }, Effects.map MatchUpdate newEffects )

    TeamUpdate a ->
      let
        ( newModel, newEffects ) =
          TeamList.update a model.teamListModel
      in
        ( { model | teamListModel = newModel }, Effects.map TeamUpdate newEffects )


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
