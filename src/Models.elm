module Models exposing(Model, initialModel)

import Material

-- MODEL


type alias Model =
    { str : String
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

initialModel : Model
initialModel =
    { str = "Hello Martinnnn"
    , mdl = Material.model
    }


-- type alias Model2 =
--     { route : Routing.Route
--     , events : List Event
--     , form : Maybe Form
--     , mdl :  Material.Model
--     }

-- initialModel : (Model2, Cmd)
-- initialModel =
--     { route = initialRoute
--     , events = []
--     , form = Nothing
--     , mdl = 
--     }