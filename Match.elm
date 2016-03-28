module Match (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Effects exposing (Effects)
import Task


type Team = 
  Team String


type Winner
  = Home
  | Away


type alias Model a =
  { home : Team 
  , away : Team
  , winner : Maybe Winner
  , informAddr : Signal.Address a 
  , toAction : (String -> String -> a)
  }


init : Signal.Address a -> (String -> String -> a) -> String -> String -> Model a
init informAddr toAction home away =
  { home = Team home
  , away = Team away
  , winner = Nothing
  , informAddr = informAddr
  , toAction = toAction
  }


type Action
  = HomeWins Team
  | AwayWins Team
  | NoOp


informEffect : Model a -> Bool -> Effects Action
informEffect model homeWin =
  let 
    home = teamName model.home
    away = teamName model.away
    act = 
      if homeWin 
      then model.toAction home away
      else model.toAction away home 
    msgTask = Signal.send model.informAddr act
  in 
    msgTask `Task.andThen` (\_ -> Task.succeed NoOp)
    |> Effects.task 


update : Action -> Model a -> (Model a, Effects Action)
update action model =
  case action of
    HomeWins _ ->
      ({ model | winner = Just Home }, informEffect model True)

    AwayWins _ ->
      ({  model | winner = Just Away }, informEffect model False)

    NoOp -> 
      (model, Effects.none )

view : Address Action -> Model a -> Html
view address model =
  div
    []
    [ div [] [ text (winnerName model) ]
    , button [ onClick address (HomeWins model.home)] [ text (teamName model.home) ]
    , button [ onClick address (AwayWins model.away)] [ text (teamName model.away) ]
    ]

teamName: Team -> String
teamName (Team team) = team
  
winnerName: Model a-> String
winnerName model =
  case model.winner of
    Just Home -> teamName model.home ++ " win at home."
    Just Away -> "The visiting "++ teamName model.away ++" win!"
    Nothing -> "You haven't bet on this match yet"
