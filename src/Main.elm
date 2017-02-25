module Main exposing (..)

import Html exposing(program)
import Messages exposing(Msg(..))
import Models exposing(Model, initialModel)
import Update exposing(update)
import View exposing(view)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }