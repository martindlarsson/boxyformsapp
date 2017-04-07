module Event.Query exposing (..)

import Task
import Models exposing (Model)
import Messages exposing (Msg(..))
import Firebase.Database
import Firebase.Database.Reference


getAllEventsQueryCmd : Model -> Cmd Msg
getAllEventsQueryCmd model =
    let
        eventsRef =
            model.db
                |> Firebase.Database.ref (Just "events")
    in
        Task.perform GotEventsMsg (Firebase.Database.Reference.once "value" eventsRef)
