module Team (..) where

import Html exposing (..)
import Signal exposing (..)


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


update : Action -> Model -> Model
update action (Team model) =
  case action of
    Lose ->
      Team { model | wins = decWins model.wins }

    Win ->
      Team { model | wins = 1 + model.wins }


decWins : Int -> Int
decWins wins =
  if wins > 0 then
    wins - 1
  else
    0


view : Address Action -> Model -> Html
view address (Team model) =
  div [] [ text ((toString model.fullName) ++ (toString model.wins)) ]
