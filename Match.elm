module Match (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)


type Team
  = Team String


type Winner
  = Home
  | Away


type Match
  = Match
      { home : Team
      , away : Team
      , winner : Maybe Winner
      }


type alias Model =
  Match


initialModel : Model
initialModel =
  Match
    { home = Team "Home"
    , away = Team "Away"
    , winner = Nothing
    }


type Action
  = HomeWins
  | AwayWins


update : Action -> Match -> Model
update action (Match match) =
  case action of
    HomeWins ->
      Match { match | winner = Just Home }

    AwayWins ->
      Match {  match | winner = Just Away }


view : Address Action -> Match -> Html
view address (Match match) =
  div
    []
    [ div [] [ text ("Winner: " ++ (toString match.winner)) ]
    , button [ onClick address HomeWins ] [ text (toString match.home) ]
    , button [ onClick address AwayWins ] [ text (toString match.away) ]
    ]
