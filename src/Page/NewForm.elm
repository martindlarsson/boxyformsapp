module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form exposing (..)


-- Msg --


type Msg
    = SaveForm
    | TextChanged Field String
    | CheckboxChanged Field Bool


type Field
    = NoField
    | FormName
    | Description
    | DateFrom
    | DateTo
    | Public


type alias Model =
    { form : Form
    , userState : UserState
    }


init : User -> Model
init user =
    { userState = validateUser (Just user)
    , form = emptyForm user
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        oldForm =
            model.form
    in
        case msg of
            SaveForm ->
                ( model, Cmd.none )

            TextChanged NoField value ->
                ( model, Cmd.none )

            TextChanged FormName value ->
                ( { model | form = { oldForm | name = value } }, Cmd.none )

            TextChanged Description value ->
                ( { model | form = { oldForm | description = value } }, Cmd.none )

            TextChanged DateFrom value ->
                ( { model | form = { oldForm | dateFrom = value } }, Cmd.none )

            TextChanged DateTo value ->
                ( { model | form = { oldForm | dateTo = value } }, Cmd.none )

            TextChanged _ value ->
                ( model, Cmd.none )

            CheckboxChanged Public value ->
                ( { model | form = { oldForm | public = value } }, Cmd.none )

            CheckboxChanged _ value ->
                ( model, Cmd.none )



-- VIEW --


view : Model -> Element Styles variation Msg
view model =
    let
        userState =
            model.userState

        form =
            model.form

        userForm =
            case (userState) of
                UserIsOK ->
                    empty

                NotLoggedIn ->
                    empty

                UserNeedsMoreInfo ->
                    FormView.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret."
    in
        column
            None
            [ spacing 20 ]
            [ paragraph H2 [] [ text "Skapa nytt formulär" ]
            , paragraph None [] [ text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
            , userForm
            , FormView.textInput Singleline "Namn" "Namnet på formuläret" form.name (TextChanged FormName) Enabled
            , FormView.textInput Multiline "Beskrivning" "Beskriv syftet med formuläret" form.description (TextChanged Description) Enabled
            , FormView.textInput Singleline "Från-datum" "Från detta datum kan användare fylla i formuläret" form.dateFrom (TextChanged DateFrom) Enabled
            , FormView.textInput Singleline "Till-datum" "Till och med detta datum kan användare fylla i formuläret" form.dateTo (TextChanged DateTo) Enabled
            , FormView.checkbox "Publikt formulär" (CheckboxChanged Public) form.public
            , FormView.button "Spara" SaveForm []
            ]
