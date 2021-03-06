port module Ports exposing (..)

import Json.Decode exposing (Value)


-- Outbound


port logOut : () -> Cmd msg


port startAuthUI : () -> Cmd msg


port addDateTimePicker : () -> Cmd msg


port saveUser : Value -> Cmd msg


port saveForm : Value -> Cmd msg


port getPublicForms : () -> Cmd msg


port getMyForms : String -> Cmd msg



-- Inbound


port userLoggedIn : (Value -> msg) -> Sub msg


port userLoggedOut : (() -> msg) -> Sub msg


port userSaved : (Value -> msg) -> Sub msg


port formSaved : (String -> msg) -> Sub msg


port gotForms : (Value -> msg) -> Sub msg
