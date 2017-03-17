module Models exposing(Model, initialModel, Route(..))

import Material
import Event.Models exposing(Event)
import Form.Models exposing(..)

-- MODEL


type alias Model =
    { events : List Event
    , route : Route
    , form : FormState
    , answers : List Answer
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

initialModel : Route -> Model
initialModel route =
    { events = []
    , route = route
    , form = NoForm
    , answers = []
    , mdl = Material.model
    }


type Route
    = EventListRoute
    | FormRoute Int
    | NotFoundRoute