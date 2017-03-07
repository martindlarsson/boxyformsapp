module Messages exposing(Msg(..))

import Material
import Event.Models exposing(Event)
import Form.Models exposing(Form)
import Navigation exposing (Location)

-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | EventFormClicked Int
    | GotEventsMsg (List Event)
    | OnLocationChange Location
    | GotFormMsg (Result String (List Form))
    | SetAnswer Int String
    | NoOp