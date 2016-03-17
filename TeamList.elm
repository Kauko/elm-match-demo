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

initialModel: {nextID: ID, teams: List (ID, Team.Model)}
initialModel =
  {teams = [(0, (Team.newTeam "Indianapolis" "Colts"))], nextID = 0}


type Action
  = Modify ID Team.Action


update : Action -> Model -> Model
update (Modify id action) model =
  { model
    | teams =
        List.map
          (\( teamID, teamModel ) ->
            if teamID == id then
              ( teamID, Team.update action teamModel )
            else
              ( teamID, teamModel )
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
