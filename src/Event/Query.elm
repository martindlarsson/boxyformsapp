module Event.Query exposing (..)

import Task
import Models exposing (Model)
import Event.Models exposing (Event)
import Messages exposing (Msg(..))
import Event.EventDecoder exposing (decodeEvents)
import Firebase.Database.Types
import Firebase.Database
import Firebase.Database.Snapshot
import Firebase.Database.Reference


getAllEventsQueryCmd : Model -> Cmd Msg
getAllEventsQueryCmd model =
    let
        eventsRef =
            model.db
                |> Firebase.Database.ref (Just "events")
    in
        Task.perform GotEventsMsg (Firebase.Database.Reference.once "value" eventsRef)


eventsSnapToEventsList : Firebase.Database.Types.Snapshot -> Result String (List Event)
eventsSnapToEventsList eventsSnapshot =
    eventsSnapshot
        |> Firebase.Database.Snapshot.value
        |> decodeEvents
