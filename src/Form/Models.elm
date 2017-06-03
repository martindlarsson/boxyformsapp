module Form.Models exposing (..)

import Helpers.ListHelpers exposing (..)
import List exposing (..)


type alias Index =
    Int


type alias FormId =
    Int


type alias FormStepId =
    String


type alias QuestionId =
    String


type alias EventId =
    Int


type alias Form =
    { eventId : EventId
    , eventName : String
    , orgName : String

    -- , formId : FormId
    , formName : String
    , formSteps : FormStepList
    }


type alias JsonForm =
    { eventId : EventId
    , eventName : String
    , orgName : String

    -- , formId : FormId
    , formName : String
    , formSteps : List FormStep
    }


type FormState
    = NoForm
    | LoadingForm
    | ErrorLoadingForm String
    | FormLoaded Form


type alias FormStepList =
    { previouseSteps : List FormStep
    , currentStep : FormStep
    , nextSteps : List FormStep
    }


type FormStepState
    = HasOneStep
    | HasNoPrevButNext
    | HasPrevButNoNext
    | HasPrevAndNext


type alias FormStep =
    { stepId : FormStepId
    , stepTitle : String
    , stepIndex : Index
    , questions : List Question
    }


type alias Question =
    { questionId : QuestionId
    , questionText : String
    , questionType : QuestionType
    , questionIndex : Int
    , choices : List Choice
    }


type QuestionType
    = TextType
    | TextType_email
    | ChoiceType
    | InfoType
    | NoType


type alias Choice =
    { choiceFee : Int
    , choiceIndex : Index
    , choiceText : String
    }


type alias Answer =
    { questionId : QuestionId
    , answer : String
    }


fromJsonFormListToForm : List JsonForm -> Result String Form
fromJsonFormListToForm jsonFormList =
    case jsonFormList of
        [] ->
            Err "Inget formulär hittades"

        firstForm :: formListTail ->
            fromJsonFormToForm firstForm


fromJsonFormToForm : JsonForm -> Result String Form
fromJsonFormToForm jsonForm =
    let
        maybeFormStepList =
            toFormStepList jsonForm.formSteps
    in
        case maybeFormStepList of
            Nothing ->
                Err "The form holds no form steps"

            Just formStepList ->
                Ok (Form jsonForm.eventId jsonForm.eventName jsonForm.orgName jsonForm.formName formStepList)


toFormStepList : List FormStep -> Maybe FormStepList
toFormStepList list =
    case list of
        [] ->
            Nothing

        firstStep :: restSteps ->
            Just (FormStepList [] firstStep restSteps)


emptyAnswer : QuestionId -> Answer
emptyAnswer qId =
    Answer qId ""


findAnswer : QuestionId -> List Answer -> Answer
findAnswer qId maybeAnswers =
    let
        maybeFoundAnswer =
            maybeAnswers
                |> List.filter (\answer -> answer.questionId == qId)
                |> List.head
    in
        case maybeFoundAnswer of
            Nothing ->
                emptyAnswer qId

            Just answer ->
                -- let
                -- _ =
                --     Debug.log "findAnswer" ("qId: " ++ qId)
                -- in
                answer


updateAnswers : List Answer -> QuestionId -> String -> List Answer
updateAnswers oldAnswers questionId answer =
    let
        newAnswer =
            [ Answer questionId answer ]

        keepAnswers =
            List.filter (\answer -> answer.questionId /= questionId) oldAnswers

        -- _ =
        --     Debug.log "updateAnsers" (keepAnswers ++ newAnswer)
    in
        keepAnswers ++ newAnswer


type MoveOperation
    = MoveNext
    | MovePreviouse
    | MovePay


moveInForm : FormState -> MoveOperation -> FormState
moveInForm formState moveOp =
    case formState of
        FormLoaded form ->
            let
                formStepList =
                    case moveOp of
                        MoveNext ->
                            nextFormStep form.formSteps

                        MovePreviouse ->
                            previouseFormStep form.formSteps

                        _ ->
                            Err "Unsupported move operation"
            in
                case formStepList of
                    Err errMsg ->
                        ErrorLoadingForm errMsg

                    Ok stepList ->
                        let
                            newForm =
                                { form | formSteps = stepList }
                        in
                            FormLoaded newForm

        _ ->
            ErrorLoadingForm "Error while loading form step"


findFormStep : Maybe FormStepId -> List FormStep -> Maybe FormStep
findFormStep formId formSteps =
    case formId of
        Nothing ->
            Nothing

        Just formId ->
            formSteps
                |> List.filter (\formStep -> formStep.stepId == formId)
                |> List.head


getCurrentStep : FormStepList -> FormStep
getCurrentStep { previouseSteps, currentStep, nextSteps } =
    currentStep


getFormStepState : FormStepList -> FormStepState
getFormStepState { previouseSteps, currentStep, nextSteps } =
    -- let
    --   _ = Debug.log "getFormStepState" ("prev: " ++ (toString (length previouseSteps)) ++ " curr: " ++ (toString currentStep) ++ " next: " ++ (toString (length nextSteps)))
    -- in
    case ( previouseSteps, currentStep, nextSteps ) of
        ( [], _, [] ) ->
            HasOneStep

        ( x :: xs, _, [] ) ->
            HasPrevButNoNext

        ( [], _, _ :: _ ) ->
            HasNoPrevButNext

        ( _ :: _, _, _ :: _ ) ->
            HasPrevAndNext


nextFormStep : FormStepList -> Result String FormStepList
nextFormStep { previouseSteps, currentStep, nextSteps } =
    let
        -- add curr last to old previouseSteps
        newPrevSteps =
            previouseSteps ++ [ currentStep ]

        -- take first from nexSteps
        newCurrStep =
            head nextSteps

        -- remove first from old nextStep
        newNextSteps =
            listTail nextSteps
    in
        case newCurrStep of
            Nothing ->
                Err "Can not go to next beqause there are no next step."

            Just currStep ->
                Ok (FormStepList newPrevSteps currStep newNextSteps)


previouseFormStep : FormStepList -> Result String FormStepList
previouseFormStep { previouseSteps, currentStep, nextSteps } =
    let
        -- remove last item from old previouseSteps
        newPrevSteps =
            dropLast previouseSteps

        -- take last from previouseSteps
        newCurrStep =
            lastInList previouseSteps

        -- add curr to old nextStep
        newNextSteps =
            [ currentStep ] ++ nextSteps
    in
        case newCurrStep of
            Nothing ->
                Err "Empty current step"

            Just currStep ->
                Ok (FormStepList newPrevSteps currStep newNextSteps)
