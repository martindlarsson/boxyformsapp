module Page.Home exposing (view, update, Msg)

import Element exposing (..)


-- import Element.Attributes exposing (..)

import BoxyStyle exposing (..)
import Views.Form as Form exposing (..)
import Data.User exposing (..)


-- Msg --


type Msg
    = NewFormMsg



-- Update --


update : Msg -> Cmd msg
update msg =
    case msg of
        NewFormMsg ->
            Cmd.none



-- VIEW --


view : Maybe User -> Element Msg
view user =
    let
        isUserDataOK =
            validateUser user

        buttonText =
            case isUserDataOK of
                UserIsOK ->
                    "Skapa nytt formulär"

                UserNeedsMoreInfo ->
                    "Fyll i mer information i min profil"

                NotLoggedIn ->
                    "Logga in och skapa ditt första formulär"
    in
        column [ spacing 20, padding 10 ]
            [ paragraph h1 [ text "Hej och välkommen till BoxyForms!" ]
            , paragraph [] [ text "Här kan du skapa formulär för t.ex. anmälan till ditt sportarrangemang eller ert bröllop. Börja med att skapa ett formulär genom att klicka på knappen nedan." ]
            , Form.button buttonText NewFormMsg [ Element.height (px 40) ]
            ]
