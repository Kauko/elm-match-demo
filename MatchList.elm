module MatchList (..) where

import Match
import Html exposing (..)
import Signal exposing (..)


matches : List ( Match.Team, Match.Team )
matches =
  [ ( Match.Team "Colts", Match.Team "Jaguars" )
  , ( Match.Team "Colts", Match.Team "Patriots" )
  ]


type alias ID =
  Int


type alias Model =
  { matches : List ( ID, Match.Model )
  , nextID : ID
  }


initialModel : Model
initialModel =
  { matches =
      List.indexedMap createMatch matches
  , nextID = List.length matches
  }


createMatch : Int -> ( Match.Team, Match.Team ) -> ( Int, Match.Match )
createMatch index ( home, away ) =
  ( index, Match.Match { home = home, away = away, winner = Nothing } )


type Action
  = Modify ID Match.Action


update : Action -> Model -> Model
update (Modify id action) model =
  { model
    | matches =
        List.map
          (\( matchId, matchModel ) ->
            if matchId == id then
              ( matchId, Match.update action matchModel )
            else
              ( matchId, matchModel )
          )
          model.matches
  }


view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map
      (\( id, matchModel ) ->
        Match.view
          (Signal.forwardTo address (Modify id))
          matchModel
      )
      model.matches
    )
