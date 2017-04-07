module Form.Query exposing (..)

import Task
import Models exposing (Model)
import Messages exposing (Msg(..))
import Firebase.Database
import Firebase.Database.Reference


getFormQueryCmd : Model -> String -> Cmd Msg
getFormQueryCmd model formId =
    let
        formsRef =
            model.db
                |> Firebase.Database.ref (Just "forms")

        formsQuery =
            formsRef
                |> Firebase.Database.Reference.child formId
                |> Firebase.Database.Reference.once "value"
    in
        Task.perform GotFormMsg formsQuery
