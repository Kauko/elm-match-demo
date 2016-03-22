module Team (..) where

import Html exposing (..)
import Signal exposing (..)
import Effects


type Team
  = Team
      { fullName : String
      , name : String
      , wins : Int
      }


type alias Model =
  Team


newTeam : String -> String -> Team
newTeam hometown name =
  Team
    { fullName = (hometown ++ " " ++ name)
    , name = name
    , wins = 0
    }


type Action
  = Win
  | Lose
  | NoOp



{- Here we need to find out:

1. Is this team playing in this match? (matchModel)
2. If yes, is this team the winner or the loser? (winner)

Match.view can't provide the teamModel for this function, so we can't
do what we need to do.
-}


toTeamAction : a -> b -> Action
toTeamAction winner matchModel =
  NoOp


update : Action -> Model -> ( Model, Effects.Effects Action )
update action (Team model) =
  case action of
    Lose ->
      ( Team { model | wins = decWins model.wins }, Effects.none )

    Win ->
      ( Team { model | wins = 1 + model.wins }, Effects.none )

    NoOp ->
      ( Team model, Effects.none )


decWins : Int -> Int
decWins wins =
  max 0 (wins - 1)


view : Address Action -> Model -> Html
view address (Team model) =
  div [] [ text ((toString model.fullName) ++ (toString model.wins)) ]
