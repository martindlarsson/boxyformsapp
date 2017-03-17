module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model, Route(..))
import Routing exposing (parseLocation)
import Material
import Ports exposing (getForm, getEvents)
import List exposing (head, tail, append)
import Form.Models exposing (Answer, QuestionId, nextFormStep, previouseFormStep, toFormStepList)
import Helpers.ListHelpers exposing (..)


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
                Ok formList ->
                    let
                        jsonForm =
                            head formList
                        
                        newForm = fromJsonFormToForm jsonForm

                    in
                        case newForm of
                            Err errMsg ->
                                ( { model | form = ErrorLoadingForm errMsg }, Cmd.none )

                            Ok form ->
                                ( { model | form = form }, Cmd.none )

                Err errorMsg ->
                    ( model, Cmd.none )

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
                formStepList = nextFormStep model.formSteps
            in
                ( { model | formSteps = formStepList }, Cmd.none )


        FormPrevButtonClicked ->
            let
                formStepList = previouseFormStep model.formSteps
            in
                ( { model | formSteps = formStepList }, Cmd.none )

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
