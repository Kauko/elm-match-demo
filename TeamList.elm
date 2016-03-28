module TeamList (..) where

import Team
import Html exposing (..)
import Signal exposing (..)
import Effects


type alias ID =
  Int


type alias Model =
  { teams : List ( ID, Team.Model )
  , nextID : ID
  }


initialModel : { nextID : ID, teams : List ( ID, Team.Model ) }
initialModel =
  { teams = [ ( 0, (Team.newTeam "Indianapolis" "Colts") ) ], nextID = 0 }


createTeams : Model -> List ( String, String ) -> Model
createTeams model teamTuples =
  let
    teams =
      List.indexedMap
        (\index ( hometown, name ) -> ( index, Team.newTeam hometown name ))
        teamTuples
  in
    { model | teams = teams, nextID = List.length teams }


type Action
  = Modify ID Team.Action
  | MatchChange String String


toTeamAction: String -> String -> Action
toTeamAction winner loser =
  MatchChange winner loser


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Modify id action ->
      let
        result =
          List.map
            (\( teamID, teamModel ) ->
              if teamID == id then
                let
                  ( newModel, newEffects ) =
                    Team.update action teamModel
                in
                  ( ( teamID, newModel ), newEffects )
              else
                ( ( teamID, teamModel ), Effects.none )
            )
            model.teams

        teams =
          List.map (\( t, _ ) -> t) result

        effects =
          Effects.batch (List.map (\( ( id, _ ), ef ) -> Effects.map (Modify id) ef) result)
      in
        ( { model | teams = teams }, effects )

    MatchChange winner loser ->
      let
        result =
          List.map
            (\( teamID, teamModel ) ->
              if teamModel.name == winner then
                let
                  ( newModel, newEffects ) =
                    Team.update Team.Win teamModel
                in
                  ( ( teamID, newModel ), newEffects )
              else if teamModel.name == loser then
                let
                  ( newModel, newEffects ) =
                    Team.update Team.Lose teamModel
                in
                  ( ( teamID, newModel ), newEffects )
              else
                ( ( teamID, teamModel ), Effects.none )
            )
            model.teams

        teams =
          List.map (\( t, _ ) -> t) result

        effects =
          Effects.batch (List.map (\( ( id, _ ), ef ) -> Effects.map (Modify id) ef) result)
      in
        ( { model | teams = teams }, effects )


view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map
      (\( id, teamModel ) ->
        Team.view
          (Signal.forwardTo address (Modify id))
          teamModel
      )
      model.teams
    )
