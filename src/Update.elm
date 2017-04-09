module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model, Route(..))
import Routing exposing (parseLocation)
import Material
import List exposing (head, tail, append)
import Form.Models exposing (..)
import Event.Query exposing (getAllEventsQueryCmd, eventsSnapToEventsList)
import Form.Query exposing (getFormQueryCmd, formSnapToForm)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAllEvents ->
            ( { model | route = EventListRoute }, getAllEventsQueryCmd model )

        GotEventsMsg eventsSnapshot ->
            let
                eventListResult =
                    eventsSnapToEventsList eventsSnapshot
            in
                case eventListResult of
                    Ok eventList ->
                        ( { model | events = eventList }, Cmd.none )

                    Err errorMsg ->
                        let
                            _ =
                                Debug.log "Error GotEventMsg" errorMsg
                        in
                            ( model, Cmd.none )

        EventFormClicked eventFormId ->
            ( { model | route = (FormRoute eventFormId) }, getFormQueryCmd model eventFormId )

        GotFormMsg formSnapshot ->
            let
                formResult =
                    formSnapToForm formSnapshot

                -- _ =
                --     Debug.log "GotFormMsg" formResult
            in
                case formResult of
                    Ok newJsonForm ->
                        let
                            newFormResult =
                                fromJsonFormToForm newJsonForm

                            -- fromJsonFormListToForm newJsonForm
                        in
                            case newFormResult of
                                Err errMsg ->
                                    ( { model | form = ErrorLoadingForm errMsg }, Cmd.none )

                                Ok form ->
                                    -- let
                                    -- _ =
                                    --     Debug.log "Jaha" "Varför händer inget?"
                                    -- in
                                    ( { model | form = FormLoaded form }, Cmd.none )

                    Err errorMsg ->
                        let
                            _ =
                                Debug.log "Err GotFormMsg" errorMsg
                        in
                            ( { model | form = ErrorLoadingForm errorMsg }, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                formState =
                    case newRoute of
                        FormRoute _ ->
                            LoadingForm

                        _ ->
                            NoForm

                command =
                    case newRoute of
                        FormRoute formId ->
                            getFormQueryCmd model formId

                        EventListRoute ->
                            if (List.isEmpty model.events) then
                                getAllEventsQueryCmd model
                            else
                                Cmd.none

                        _ ->
                            Cmd.none

                -- _ = Debug.log "OnLocationChange" newRoute
            in
                ( { model | route = newRoute, form = formState }, command )

        SetAnswer questionId answer ->
            let
                -- _ =
                --     Debug.log "SetAnswer" ((toString questionId) ++ ", svar: " ++ answer)
                newAnsers =
                    updateAnswers model.answers questionId answer
            in
                ( { model | answers = newAnsers }, Cmd.none )

        FormNextButtonClicked ->
            let
                oldFormState =
                    model.form

                newFormState =
                    moveInForm oldFormState MoveNext
            in
                ( { model | form = newFormState }, Cmd.none )

        FormPrevButtonClicked ->
            let
                oldFormState =
                    model.form

                newFormState =
                    moveInForm oldFormState MovePreviouse
            in
                ( { model | form = newFormState }, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Mdl subMsg ->
            Material.update Mdl subMsg model

        NoOp ->
            ( model, Cmd.none )
