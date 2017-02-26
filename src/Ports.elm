port module Ports exposing(..)

import Event.Models exposing(Event)

port getEvents : () -> Cmd msg

port gotEventList : ( List Event -> msg) -> Sub msg

