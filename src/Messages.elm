module Messages exposing(Msg(..))

import Material
import Event.Models exposing(Event)
import Navigation exposing (Location)

-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | EventFormClicked Int
    | GotEventsMsg (List Event)
    | OnLocationChange Location
    | NoOp