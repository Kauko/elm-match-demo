module Team (..) where

import Html exposing (..)
import Signal exposing (..)


type alias Model =
  { fullName : String
  , name : String
  , wins : Int
  }

isTeam : String -> Model -> Bool
isTeam name team =
  team.name == name 

newTeam : String -> String -> Model
newTeam hometown name =
  { fullName = (hometown ++ " " ++ name)
  , name = name
  , wins = 0
  }


type Action
  = Win
  | Lose


win : Model -> Model
win = update Win 

lose : Model -> Model
lose = update Lose

update : Action -> Model -> Model
update action model =
  case action of
    Lose ->
      { model | wins = decWins model.wins }

    Win ->
      { model | wins = 1 + model.wins }

decWins: Int -> Int
decWins wins =
  max 0 (wins - 1)


view : Address Action -> Model -> Html
view address model =
  div [] [ text ((toString model.fullName) ++ (toString model.wins)) ]
