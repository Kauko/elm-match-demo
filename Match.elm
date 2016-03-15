module Match (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)


type Team
  = Team String


type Winner
  = Home
  | Away


type alias Model =
  { home : Team
  , away : Team
  , winner : Team
  }


initialModel : Model
initialModel =
  { home = Team "Home"
  , away = Team "Away"
  , winner = Team "Home"
  }


type Action
  = HomeWins
  | AwayWins


update : Action -> Model -> Model
update action match =
  case action of
    HomeWins ->
      { match | winner = match.home }

    AwayWins ->
      { match | winner = match.away }


view : Address Action -> Model -> Html
view address match =
  div
    []
    [ div [] [text ("Winner: " ++ (toString match.winner))]
    , button [ onClick address HomeWins ] [ text (toString match.home) ]
    , button [ onClick address AwayWins ] [ text (toString match.away) ]
    ]
