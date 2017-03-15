module Update exposing (..)

import Messages exposing (Msg(..))
import Models exposing (Model, Route(..))
import Routing exposing (parseLocation)
import Material
import Ports exposing (getForm, getEvents)
import List exposing (head, tail)
import Form.Models exposing (Answer, QuestionId)


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
                        form =
                            head formList
                        
                        firstFormStep =
                            case form of
                                Nothing -> Nothing
                                
                                Just form -> head form.formSteps

                        formStepsTail =
                            case form of
                                Nothing -> Nothing
                                
                                Just form -> tail form.formSteps
                    in
                        ( { model | form = form
                            , currentFormStep = firstFormStep
                            , formStepsTail = formStepsTail }, Cmd.none )

                Err errorMsg ->
                    ( model, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

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
                ( { model | route = newRoute }, command )

        SetAnswer questionId answer ->
            let
                -- _ = Debug.log "SetAnswer" ((toString questionId) ++ ", svar: " ++ answer )
                newAnsers =
                    updateAnswers model questionId answer
            in
                ( { model | answers = newAnsers }, Cmd.none )

        FormNextButtonClicked ->
            let
                listHead =
                    case model.formStepsHead of
                        Nothing -> [ model.currentFormStep ]

                        Just steps -> steps :: [ model.currentFormStep ]

                currentStep =
                    case model.formStepsTail of
                        Nothing -> Nothing

                        Just steps -> head steps

                listTail =
                    case model.formStepsTail of
                        Nothing -> Nothing

                        Just steps -> tail steps
            in
                        ( { model | formStepsHead = listHead
                            , currentFormStep = currentStep
                            , formStepsTail = listTail }, Cmd.none )

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
