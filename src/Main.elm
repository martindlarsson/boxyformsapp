module Main exposing (..)

import Messages exposing(Msg(..))
import Models exposing(Model, initialModel, Route(..))
import Update exposing(update)
import View exposing(view)
import Ports exposing(..)
import Navigation exposing (Location)
import Routing
import Form.FormDecoder exposing(formDecoder)
import Json.Decode exposing(decodeValue, list)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotEventList GotEventsMsg
        , gotForm (GotFormMsg << decodeValue formDecoder)
        ]



init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location
    in
        case currentRoute of
            EventListRoute ->
                ( initialModel currentRoute, getEvents () )

            FormRoute formId ->
                ( initialModel currentRoute, getForm formId )

            _ ->
                ( initialModel currentRoute, Cmd.none )

-- MAIN


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }