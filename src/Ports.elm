port module Ports exposing (..)

import Json.Decode exposing (Value)
import Data.Form exposing (Form, FormId)


-- Session


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg



-- Forms


port getAllPublicForms : () -> Cmd msg


port gotAllPublicForms : (Json.Decode.Value -> msg) -> Sub msg


port getForm : FormId -> Cmd msg


port gotForm : (Json.Decode.Value -> msg) -> Sub msg



-- Authentication
-- Outbound


port requestAuthentication : String -> Cmd msg


port logOut : () -> Cmd msg


port startAuthUI : () -> Cmd msg


port deleteFBUI : () -> Cmd msg


port addDateTimePicker : () -> Cmd msg



-- Inbound


port receiveUser : (Value -> msg) -> Sub msg
