module Page.Login exposing (Model, initialModel, view)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Data.Session as Session exposing (Session)


-- MODEL --


type alias Model =
    { errors : List String
    }


initialModel : Model
initialModel =
    { errors = []
    }


view : Session -> Model -> Html msg
view session model =
    div [ class "container" ]
        [ span [] [ text "Har du inget konto redan skapas det en användare när du loggar in första gången." ]
        , div [ id "firebaseui-auth-container" ] []
        ]
