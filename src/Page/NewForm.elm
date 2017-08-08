module Page.NewForm exposing (view, initialModel, Model, Msg, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data.Form as Form exposing (Form)
import Data.Session as Session exposing (Session)
import Views.Form as FormViews


-- MODEL --


type alias Model =
    { newForm : Form
    , errors : List String
    }


initialModel : Model
initialModel =
    { newForm =
        { id = "new"
        , name = "Nytt formulär"
        , description = ""
        , openDate = ""
        , closeDate = ""
        , public = False
        , imgUrl = ""
        , orgName = ""
        , orgId = ""
        }
    , errors = []
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "container page" ]
        [ div [ class "row" ]
            [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                [ h1 [ class "text-xs-center" ] [ text "Nytt formulär" ]

                -- , FormViews.viewErrors model.errors
                , editFormView model.newForm
                ]
            ]
        ]


editFormView : Form -> Html Msg
editFormView form =
    Html.form [ onSubmit SubmitForm ]
        [ FormViews.input
            [ class "form-control-lg"
            , placeholder "Formulärets namn"
            , onInput (OnInput Name)
            ]
            []
        , FormViews.textarea
            [ class "form-control-lg"
            , placeholder "Beskrivning"
            , onInput (OnInput Description)
            ]
            []
        , FormViews.input
            [ class "form-control-lg"
            , placeholder "Datum när formuläret blir tillgängligt"
            , onInput (OnInput OpenDate)
            ]
            []
        , FormViews.input
            [ class "form-control-lg"
            , placeholder "Datum när formuläret stänger"
            , onInput (OnInput CloseDate)
            ]
            []
        , FormViews.input
            [ class "form-control-lg"
            , placeholder "Publikt"
            , onInput (OnInput Public)
            ]
            []
        , FormViews.input
            [ class "form-control-lg"
            , placeholder "URL till bild"
            , onInput (OnInput ImgURL)
            ]
            []
        , button [ class "btn btn-lg btn-secondary pull-right" ]
            [ text "Nästa" ]
        ]



-- UPDATE --


type InputField
    = Name
    | Description
    | OpenDate
    | CloseDate
    | Public
    | ImgURL


type Msg
    = SubmitForm
    | OnInput InputField String


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        _ =
            Debug.log "update NewForm" msg
    in
        case msg of
            SubmitForm ->
                ( model, Cmd.none )

            OnInput inputField inputString ->
                let
                    oldForm =
                        model.newForm
                in
                    case inputField of
                        Name ->
                            ( { model | newForm = { oldForm | name = inputString } }, Cmd.none )

                        Description ->
                            ( { model | newForm = { oldForm | description = inputString } }, Cmd.none )

                        OpenDate ->
                            ( { model | newForm = { oldForm | openDate = inputString } }, Cmd.none )

                        CloseDate ->
                            ( { model | newForm = { oldForm | closeDate = inputString } }, Cmd.none )

                        Public ->
                            -- TODO, konvertera string -> bool
                            ( { model | newForm = { oldForm | public = False } }, Cmd.none )

                        ImgURL ->
                            ( { model | newForm = { oldForm | imgUrl = inputString } }, Cmd.none )
