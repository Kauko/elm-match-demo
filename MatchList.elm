module MatchList (..) where

import Match
import Html exposing (..)
import Signal exposing (..)


type alias ID =
  Int


type alias Model =
  { matches : List ( ID, Match.Model )
  , nextID : ID
  }


initialModel : Model
initialModel =
  { matches = []
  , nextID = 0
  }


createMatch : Int -> ( String, String ) -> ( Int, Match.Match )
createMatch index ( home, away ) =
  ( index
  , Match.Match
      { home = (Match.Team home)
      , away = (Match.Team away)
      , winner = Nothing
      }
  )


createMatches : Model -> List ( String, String ) -> Model
createMatches model matchups =
  let
    matches =
      List.indexedMap createMatch matchups
  in
    { model | matches = matches, nextID = List.length matches }


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


view : Address Action -> Address a  -> Model -> Html
view address teamAddress model =
  div
    []
    (List.map
      (\( id, matchModel ) ->
        Match.view
          (Signal.forwardTo address (Modify id)) teamAddress
          matchModel
      )
      model.matches
    )
