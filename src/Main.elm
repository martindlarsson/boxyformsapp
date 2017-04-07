module Main exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model, initialModel, Route(..))
import Update exposing (update)
import View exposing (view)
import Navigation exposing (Location)
import Routing
import Event.Query exposing (getAllEventsQueryCmd)
import Form.Query exposing (getFormQueryCmd)


-- SUBSCRIPTIONS
-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.batch
--         [ gotEventList GotEventsMsg
--         , gotForm (GotFormMsg << decodeValue formDecoder)
--         ]


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location

        model =
            initialModel currentRoute
    in
        case currentRoute of
            EventListRoute ->
                ( model, (getAllEventsQueryCmd model) )

            FormRoute formId ->
                ( model, getFormQueryCmd model formId )

            _ ->
                ( model, Cmd.none )



-- MAIN


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none -- subscriptions
        }
