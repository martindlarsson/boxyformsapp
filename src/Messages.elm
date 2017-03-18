module Messages exposing(Msg(..))

import Material
import Event.Models exposing(Event)
import Form.Models exposing(JsonForm)
import Navigation exposing (Location)

-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | EventFormClicked Int
    | GotEventsMsg (List Event)
    | OnLocationChange Location
    | GotFormMsg (Result String (List JsonForm))
    | SetAnswer Int String
    | FormNextButtonClicked
    | FormPrevButtonClicked
    | NoOp