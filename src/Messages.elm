module Messages exposing(Msg(..))

import Material

-- MESSAGES


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp