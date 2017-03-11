module Models exposing(Model, initialModel, Route(..))

import Material
import Event.Models exposing(Event)
import Form.Models exposing(Form, Answer)

-- MODEL


type alias Model =
    { events : List Event
    , route : Route
    , form : Maybe Form
    , currentFormStep : Maybe Int
    , answers : List Answer
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

initialModel : Route -> Model
initialModel route =
    { events = []
    , route = route
    , form = Nothing
    , currentFormStep = Nothing
    , answers = []
    , mdl = Material.model
    }


type Route
    = EventListRoute
    | FormRoute Int
    | NotFoundRoute