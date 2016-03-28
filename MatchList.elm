module MatchList (..) where

import Match
import Html exposing (..)
import Signal exposing (..)

import Effects exposing (Effects)


type alias ID =
  Int


type alias Model a = 
  { matches : List ( ID, Match.Model a)
  , nextID : ID
  }


initialModel : Signal.Address a -> (String -> String -> a) -> List ( String, String ) -> Model a
initialModel informAddr toAction matchups =
  { matches = List.indexedMap (createMatch informAddr toAction) matchups
  , nextID = 0

  }


createMatch : Signal.Address a -> (String -> String -> a) -> Int -> ( String, String ) -> ( Int, Match.Model a )
createMatch informAddr toAction index ( home, away ) =
  ( index
  , Match.init informAddr toAction home away
  )


type Action
  = Modify ID Match.Action


update : Action -> Model a -> ( Model a, Effects Action)
update (Modify id action) model =
  let 
    matches_and_fx = 
      List.map
        (\( matchId, matchModel ) ->
          if matchId == id then
            let 
              (matchModel', fx) = Match.update action matchModel
            in
              (( matchId,  matchModel'), Effects.map (Modify id) fx)
          else
            (( matchId,  matchModel), Effects.none )
        )
        model.matches
  in 
    ( { model | matches = List.map fst matches_and_fx}
    , Effects.batch <| List.map snd matches_and_fx)


view : Address Action -> Model a -> Html
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
