module Models exposing(Model, initialModel)

import Material
import Event.Models exposing(Event)

-- MODEL


type alias Model =
    { events : List Event
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

initialModel : Model
initialModel =
    { events = []
    , mdl = Material.model
    }