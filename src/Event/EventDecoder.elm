module Event.EventDecoder exposing (..)

import Event.Models exposing (..)
import Json.Decode exposing (int, string, bool, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required)


decodeEvents : Json.Decode.Value -> Result String (List Event)
decodeEvents modelJson =
    Json.Decode.decodeValue decodeEventList modelJson


decodeEventList : Decoder (List Event)
decodeEventList =
    list eventDecoder


eventDecoder : Decoder Event
eventDecoder =
    decode Event
        |> required "eventId" int
        |> required "eventName" string
        |> required "eventForms" (list eventFormDecoder)
        |> required "organizer" string
        |> required "organizerId" int
        |> required "public" bool
        |> required "webLink" string


eventFormDecoder : Decoder EventForm
eventFormDecoder =
    decode EventForm
        |> required "formId" string
        |> required "formName" string
