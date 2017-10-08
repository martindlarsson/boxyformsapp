module Page.NewForm exposing (view, initialModel, Model, Msg, update, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data.Form as Form exposing (Form)
import Data.Session as Session exposing (Session)
import Views.Form as FormViews
import Date exposing (Date)


-- import Date.Extra.Config.Config_en_us exposing (config)
-- import Date.Extra.Format
-- import DateParser

import DateTimePicker
import DateTimePicker.Css
import DateTimePicker.Config exposing (Config, DatePickerConfig, TimePickerConfig, defaultDatePickerConfig, defaultDateTimeI18n, defaultDateTimePickerConfig, defaultTimePickerConfig)
import Css


-- import Html.CssHelpers
-- MODEL --


type alias Model =
    { newForm : Form
    , errors : List String
    , dateOpen : Maybe Date
    , dateOpenPickerState : DateTimePicker.State
    , dateClose : Maybe Date
    , dateClosePickerState : DateTimePicker.State
    }


digitalDateOpenTimePickerConfig : Config (DatePickerConfig TimePickerConfig) Msg
digitalDateOpenTimePickerConfig =
    let
        defaultDateTimeConfig =
            defaultDateTimePickerConfig DateOpenChanged
    in
        { defaultDateTimeConfig
            | timePickerType = DateTimePicker.Config.Digital
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
    , dateOpen = Nothing
    , dateOpenPickerState = DateTimePicker.initialState
    , dateClose = Nothing
    , dateClosePickerState = DateTimePicker.initialState
    }


init : Session -> Cmd Msg
init session =
    Cmd.batch
        [ DateTimePicker.initialCmd DateOpenChanged DateTimePicker.initialState
        , DateTimePicker.initialCmd DateCloseChanged DateTimePicker.initialState
        ]



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "container page" ]
        [ div [ class "row" ]
            [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                [ h1 [ class "text-xs-center" ] [ text "Nytt formulär" ]

                -- , FormViews.viewErrors model.errors
                , editFormView model
                ]
            ]
        ]


editFormView : Model -> Html Msg
editFormView model =
    let
        { css } =
            Css.compile [ DateTimePicker.Css.css ]

        form =
            model.newForm
    in
        Html.form [ onSubmit SubmitForm ]
            [ Html.node "style" [] [ Html.text css ]
            , FormViews.input
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
            , label []
                [ text "Startdatum"
                , DateTimePicker.dateTimePickerWithConfig
                    digitalDateOpenTimePickerConfig
                    []
                    model.dateOpenPickerState
                    model.dateOpen
                ]

            -- , div [ class "form-control" ]
            --     [ div [ id "datepicker", class "date", onInput (OnInput OpenDate) ] []
            --     ]
            -- , FormViews.dateTimePicker
            --     [ class "form-control-lg"
            --     , placeholder "Datum när formuläret blir tillgängligt"
            --     , onInput (OnInput OpenDate)
            --     ]
            -- , FormViews.dateTimePicker
            --     [ class "form-control-lg"
            --     , placeholder "Datum när formuläret stänger"
            --     , onInput (OnInput CloseDate)
            --     ]
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



-- Used to identify which date time picker is calling


type DateTimePickerInstance
    = DateClosePicker
    | DateOpenPicker


type Msg
    = SubmitForm
    | OnInput InputField String
    | DateOpenChanged DateTimePicker.State (Maybe Date)
    | DateCloseChanged DateTimePicker.State (Maybe Date)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    -- let
    --     _ =
    --         Debug.log "update NewForm" msg
    -- in
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

        DateOpenChanged pickerState newDate ->
            ( { model | dateOpen = newDate, dateOpenPickerState = pickerState }, Cmd.none )

        DateCloseChanged pickerState newDate ->
            ( { model | dateClose = newDate, dateClosePickerState = pickerState }, Cmd.none )
