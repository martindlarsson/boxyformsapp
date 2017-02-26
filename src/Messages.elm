module Messages exposing(Msg(..))

import Material
import Event.Models exposing(Event)

-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | EventClicked
    | GotEventsMsg (List Event)
    | NoOp