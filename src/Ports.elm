port module Ports exposing(..)

import Event.Models exposing(Event)
import Form.Models exposing(Form, FormId)
import Json.Decode exposing (Value)

-- Events

port getEvents : () -> Cmd msg

port gotEventList : ( List Event -> msg) -> Sub msg


-- Forms

port getForm : FormId -> Cmd msg

port gotForm : ( List Json.Decode.Value -> msg ) -> Sub msg
