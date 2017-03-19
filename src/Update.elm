module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model, Route(..))
import Routing exposing (parseLocation)
import Material
import Ports exposing (getForm, getEvents)
import List exposing (head, tail, append)
import Form.Models exposing (..)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEventsMsg eventsList ->
            -- let
            --     _ = Debug.log "GotEventsMsg" eventsList
            -- in
            ( { model | events = eventsList }, Cmd.none )

        EventFormClicked eventFormId ->
            let
                _ =
                    Debug.log "EventFormClicked" eventFormId
            in
                ( model, getForm eventFormId )

        GotFormMsg formResult ->
            case formResult of
                Ok newJsonForm ->
                    let
                        newForm = fromJsonFormToForm newJsonForm
                    in
                        case newForm of
                            Err errMsg ->
                                ( { model | form = ErrorLoadingForm errMsg }, Cmd.none )

                            Ok form ->
                                ( { model | form = FormLoaded form }, Cmd.none )

                Err errorMsg ->
                let
                  _ = Debug.log "Err GotFormMsg" errorMsg
                in
                    ( { model | form = ErrorLoadingForm errorMsg }, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                formState =
                    case newRoute of
                        FormRoute _ -> LoadingForm

                        _ -> NoForm

                command =
                    case newRoute of
                        FormRoute formId ->
                            getForm formId

                        EventListRoute ->
                            if (List.isEmpty model.events) then
                                getEvents ()
                            else
                                Cmd.none

                        _ ->
                            Cmd.none

                -- _ = Debug.log "OnLocationChange" newRoute
            in
                ( { model | route = newRoute, form = formState }, command )

        SetAnswer questionId answer ->
            let
                -- _ = Debug.log "SetAnswer" ((toString questionId) ++ ", svar: " ++ answer )
                newAnsers =
                    updateAnswers model questionId answer
            in
                ( { model | answers = newAnsers }, Cmd.none )

        FormNextButtonClicked ->
            let
                formState = model.form
            in
                case formState of
                    FormLoaded form ->
                        let
                            formStepList = nextFormStep form.formSteps
                        in
                            case formStepList of
                                Err errMsg ->
                                    ( { model | form = (ErrorLoadingForm errMsg) }, Cmd.none )

                                Ok stepList ->
                                    let
                                        newForm = { form | formSteps = stepList }

                                        newFormState = FormLoaded newForm
                                    in
                                        ( { model | form = newFormState }, Cmd.none )

                    _ ->
                        let
                            _ = Debug.log "Error in FormNextButtonClicked" (toString formState)
                        in
                            ( { model | form = (ErrorLoadingForm "Error while loading form step") }, Cmd.none )


        FormPrevButtonClicked ->
            let
                formState = model.form
            in
                case formState of
                    FormLoaded form ->
                        let
                            formStepList = previouseFormStep form.formSteps
                        in
                            case formStepList of
                                Err errMsg ->
                                    ( { model | form = (ErrorLoadingForm errMsg) }, Cmd.none )

                                Ok stepList ->
                                    let
                                        newForm = { form | formSteps = stepList }

                                        newFormState = FormLoaded newForm
                                    in
                                        ( { model | form = newFormState }, Cmd.none )

                    _ ->
                        let
                            _ = Debug.log "Error in FormPrevButtonClicked" (toString formState)
                        in
                            ( { model | form = (ErrorLoadingForm "Error while loading form step") }, Cmd.none )


        -- Boilerplate: Mdl action handler.
        Mdl subMsg ->
            Material.update Mdl subMsg model

        NoOp ->
            ( model, Cmd.none )


updateAnswers : Model -> QuestionId -> String -> List Answer
updateAnswers model questionId answer =
    let
        newAnswer =
            [ Answer questionId answer ]

        oldAnswers =
            model.answers

        keepAnswers =
            List.filter (\answer -> answer.questionId /= questionId) oldAnswers

        _ =
            Debug.log "updateAnsers" (keepAnswers ++ newAnswer)
    in
        keepAnswers ++ newAnswer
