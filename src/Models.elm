module Models exposing(Model, initialModel, Route(..))

import Material
import Event.Models exposing(Event)
import Form.Models exposing(Form, FormStep, Answer)

-- MODEL


type alias Model =
    { events : List Event
    , route : Route
    , form : Maybe Form
    , formStepsHead : Maybe (List FormStep)
    , currentFormStep : Maybe FormStep
    , formStepsTail : Maybe (List FormStep)
    , answers : List Answer
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }

initialModel : Route -> Model
initialModel route =
    { events = []
    , route = route
    , form = Nothing
    , formStepsHead = Nothing
    , currentFormStep = Nothing
    , formStepsTail = Nothing
    , answers = []
    , mdl = Material.model
    }


type Route
    = EventListRoute
    | FormRoute Int
    | NotFoundRoute