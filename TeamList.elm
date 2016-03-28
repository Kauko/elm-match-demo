module TeamList (..) where

import Team
import Html exposing (..)
import Signal exposing (..)


type alias ID =
  Int


type alias Model =
  { teams : List ( ID, Team.Model )
  , nextID : ID
  }


initialModel : List ( String, String ) -> Model
initialModel teamTuples =
  let
    teams =
      List.indexedMap
        (\index ( hometown, name ) -> ( index, Team.newTeam hometown name ))
        teamTuples
  in 
    Model teams (List.length teams)


type Action
  = Modify ID Team.Action
  | UpdateOnMatch String String

updateTeamsAction : String -> String -> Action
updateTeamsAction = UpdateOnMatch


update : Action -> Model -> Model
update action model =
  case action of 
    Modify id act ->
      { model
        | teams =
            List.map
              (\( teamID, teamModel ) ->
                if teamID == id then
                  ( teamID, Team.update act teamModel )
                else
                  ( teamID, teamModel )
              )
              model.teams
      }
    UpdateOnMatch winner loser -> 
      { model
        | teams =
            List.map
              (\( teamID, teamModel ) ->
                if Team.isTeam winner teamModel
                then 
                  ( teamID, Team.win teamModel )
                else 
                  if Team.isTeam loser teamModel
                  then 
                    ( teamID, Team.lose teamModel )
                  else
                    ( teamID, teamModel ) -- not part of the win-lose
              )
              model.teams
      }

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
