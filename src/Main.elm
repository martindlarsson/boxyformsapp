module Main exposing (..)

-- import Html exposing(program)
import Messages exposing(Msg(..))
import Models exposing(Model, initialModel)
import Update exposing(update)
import View exposing(view)
import Ports exposing(..)
import Navigation exposing (Location)
import Routing

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotEventList GotEventsMsg
        ]



init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        ( initialModel currentRoute, getEvents () )

-- MAIN


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }