module Match (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Effects
import TeamList


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
  = HomeWins Team
  | AwayWins Team


update : Action -> Match -> ( Model, Effects.Effects Action )
update action (Match match) =
  case action of
    HomeWins _ ->
      ( Match { match | winner = Just Home }, Effects.none )

    AwayWins _ ->
      ( Match { match | winner = Just Away }, Effects.none )


view : Address Action -> Address TeamList.Action -> Match -> Html
view address teamAddress (Match match) =
  div
    []
    [ div [] [ text (winnerName (Match match)) ]
    , button
        -- Can't have two onClick handlers!
        [ onClick address (HomeWins match.home) ]
        [ text (teamName match.home) ]
    , button [ onClick address (AwayWins match.away) ] [ text (teamName match.away) ]
    ]


teamName : Team -> String
teamName team =
  case team of
    Team str ->
      str


winnerName : Match -> String
winnerName (Match match) =
  case match.winner of
    Just Home ->
      teamName match.home ++ " win at home."

    Just Away ->
      "The visiting " ++ teamName match.away ++ " win!"

    Nothing ->
      "You haven't bet on this match yet"
