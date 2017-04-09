module Messages exposing (Msg(..))

import Material
import Navigation exposing (Location)
import Firebase.Database.Types
import Form.Models exposing (QuestionId)


-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | EventFormClicked String
    | GetAllEvents -- Nödvändig?
    | GotEventsMsg Firebase.Database.Types.Snapshot -- (List Event)
    | OnLocationChange Location
    | GotFormMsg Firebase.Database.Types.Snapshot -- (Result String JsonForm)
    | SetAnswer QuestionId String
    | FormNextButtonClicked
    | FormPrevButtonClicked
    | NoOp
