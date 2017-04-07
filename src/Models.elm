module Models exposing (Model, initialModel, Route(..))

import Material
import Event.Models exposing (Event)
import Form.Models exposing (..)
import Firebase
import Firebase.Database
import Firebase.Database.Types


-- MODEL


type alias Model =
    { app : Firebase.App
    , db : Firebase.Database.Types.Database
    , events : List Event -- Gör om till union type som FormState
    , route : Route
    , form : FormState
    , answers : List Answer
    , mdl : Material.Model -- Boilerplate: model store for any and all Mdl components you use.
    }


initialModel : Route -> Model
initialModel route =
    let
        fbApp =
            Firebase.init
                { apiKey = "AIzaSyCWZWSkUj-3HQdHhk6xGsxvn0Me8eD6KWU"
                , databaseURL = "https://boxyformsdb.firebaseio.com"
                , authDomain = "boxyformsdb.firebaseapp.com"
                , storageBucket = "boxyformsdb.appspot.com"
                , messagingSenderId = "133792302370"
                }

        fbDb =
            Firebase.Database.init fbApp
    in
        { app = fbApp
        , db = fbDb
        , events = []
        , route = route
        , form = NoForm
        , answers = []
        , mdl = Material.model
        }


type Route
    = EventListRoute
    | FormRoute String
    | NotFoundRoute
