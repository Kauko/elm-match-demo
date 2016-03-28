module Team (..) where

import Html exposing (..)
import Signal exposing (..)
import Effects


type alias Model =
  { fullName : String
  , name : String
  , wins : Int
  }


newTeam : String -> String -> Model
newTeam hometown name =
  { fullName = (hometown ++ " " ++ name)
  , name = name
  , wins = 0
  }


type Action
  = Win
  | Lose
  | NoOp


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Lose ->
      ( { model | wins = decWins model.wins }, Effects.none )

    Win ->
      ( { model | wins = 1 + model.wins }, Effects.none )

    NoOp ->
      ( model, Effects.none )


decWins : Int -> Int
decWins wins =
  max 0 (wins - 1)


view : Address Action -> Model -> Html
view address model =
  div [] [ text ((toString model.fullName) ++ (toString model.wins)) ]
