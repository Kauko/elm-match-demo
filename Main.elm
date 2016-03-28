module Main (..) where

import Html exposing (..)
import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)

import MatchList

import TeamList
import Signal exposing (..)

noFx : a -> ( a, Effects b )
noFx m = (m, Effects.none)

routing : Mailbox Action
routing = Signal.mailbox NoOp

app : StartApp.App Model
app = StartApp.start
  { init = noFx initialModel
  , view = view
  , update = update
  , inputs = [routing.signal]
  }



main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks  

type alias Model =
  { teamListModel : TeamList.Model
  , matchListModel : MatchList.Model TeamList.Action
  }


initialModel : Model
initialModel = 
  let 
    teamListAddr = Signal.forwardTo routing.address TeamUpdate
    matchListAddr = Signal.forwardTo routing.address MatchUpdate

  in 
    { matchListModel =
        MatchList.initialModel teamListAddr TeamList.updateTeamsAction
          [( "Colts", "Patriots" ), ( "Colts", "Jaguars" ) ]
    , teamListModel = 
        TeamList.initialModel
          [("Indianapolis", "Colts"), ("Jacksonville", "Jaguars")]
    }


type Action
  = MatchUpdate MatchList.Action
  | TeamUpdate TeamList.Action
  | NoOp


update : Action -> Model -> ( Model, Effects Action)
update action model =
  case action of
    MatchUpdate a ->
      let
        (matchListModel', fx) = MatchList.update a model.matchListModel
      in
        ({ model | matchListModel = matchListModel'}, Effects.map MatchUpdate fx)

    TeamUpdate a ->
      noFx { model | teamListModel = TeamList.update a model.teamListModel }

    NoOp -> noFx model

view : Address Action -> Model -> Html
view address model =
  div
    []
    [ TeamList.view (Signal.forwardTo address TeamUpdate) model.teamListModel
    , MatchList.view (Signal.forwardTo address MatchUpdate) model.matchListModel
    ]
