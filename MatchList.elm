module MatchList (..) where

import Match
import Html exposing (..)
import Signal exposing (..)
import Effects
import TeamList


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


update : Action -> Model -> ( Model, Effects.Effects Action )
update (Modify id action) model =
  let
    result =
      List.map
        (\( matchID, matchModel ) ->
          if matchID == id then
            let
              ( newModel, newEffects ) =
                Match.update action matchModel
            in
              ( ( matchID, newModel ), newEffects )
          else
            ( ( matchID, matchModel ), Effects.none )
        )
        model.matches

    matches =
      List.map (\( m, _ ) -> m) result

    effects =
      Effects.batch (List.map (\( ( id, _ ), e ) -> Effects.map (Modify id) e) result)
  in
    ( { model | matches = matches }, effects )


view : Address Action -> Address TeamList.Action -> Model -> Html
view address teamAddress model =
  div
    []
    (List.map
      (\( id, matchModel ) ->
        Match.view
          (Signal.forwardTo address (Modify id))
          teamAddress
          matchModel
      )
      model.matches
    )
