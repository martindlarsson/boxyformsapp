module Form.Models exposing (..)

import Helpers.ListHelpers exposing (..)
import List exposing (..)

type alias Index =
    Int


type alias FormId =
    Int


type alias FormStepId =
    Int


type alias QuestionId =
    Int


type alias EventId =
    Int


type alias Form =
    { eventId : EventId
    , eventName : String
    , orgName : String
    , formId : FormId
    , formName : String
    , formSteps : FormStepList
    }


type alias JsonForm =
    { eventId : EventId
    , eventName : String
    , orgName : String
    , formId : FormId
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
    = Loading
    | HasOneStep
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


fromJsonFormToForm : JsonForm -> Result String Form
fromJsonFormToForm jsonForm =
    let
        firstStep =
            case (head jsonForm.formSteps) of
                Nothing -> Err "The form holds no form steps"

                Just formStep -> formStep

        formStepList =
            { previouseSteps = []
            , currentStep = firstStep
            , nextSteps = (tail jsonForm.formSteps)
            }
    in
        Form
            { eventId = jsonForm.eventId
            , eventName = jsonForm.eventName
            , orgName = jsonForm.orgName
            , formId = jsonForm.formId
            , formName = jsonForm.formName
            , formSteps = formStepList
            }


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
                answer


findFormStep : Maybe FormStepId -> List FormStep -> Maybe FormStep
findFormStep formId formSteps =
    case formId of
        Nothing -> Nothing

        Just formId ->
            formSteps
                |> List.filter (\formStep -> formStep.stepId == formId)
                |> List.head


-- toFormStepList : List FormStep -> FormStepList
-- toFormStepList list =
--     case list of
--         [] -> NoSteps

--         firstStep :: restSteps ->
--             Steps { previouseSteps = [], currentStep = firstStep, nextSteps = restSteps }


getCurrentStep : FormStepList -> FormStep
getCurrentStep { previouseSteps, currentStep, nextSteps } =
    currentStep

getFormStepState : FormStepList -> FormStepState
getFormStepState { previouseSteps, currentStep, nextSteps } =
    case (previouseSteps, currentStep, nextSteps ) of
        ( [], _ , [] ) -> HasOneStep

        ( [ items ], _ , [] ) -> HasPrevButNoNext

        ( [], _ , [ items ] ) -> HasNoPrevButNext

        ( [ items ], _ , [ items2 ] ) -> HasPrevAndNext

        _ -> Loading


nextFormStep : FormStepList -> Result String FormStepList
nextFormStep { previouseSteps, currentStep, nextSteps } =
    let
        -- add curr last to old previouseSteps
        newPrevSteps = previouseSteps ++ [ currentStep ]

        -- take first from nexSteps
        newCurrStep = head nextSteps

        -- remove first from old nextStep
        newNextSteps = listTail nextSteps
    in
        case newCurrStep of
            Nothing ->
                Err "Can not go to next beqause there are no next step."

            Just currStep ->
                Ok ( FormStepList { previouseSteps = newPrevSteps, currentStep = newCurrStep, nextSteps = newNextSteps })


previouseFormStep : FormStepList -> Result String FormStepList
previouseFormStep { previouseSteps, currentStep, nextSteps } =
    let
        -- remove last item from old previouseSteps
        newPrevSteps = dropLast previouseSteps

        -- take last from previouseSteps
        newCurrStep = lastInList previouseSteps

        -- add curr to old nextStep
        newNextSteps = [ currentStep ] ++ nextSteps
    in
        case newCurrStep of
            Nothing ->
                Err "Empty current step"

            Just currStep ->
                Ok ( FormStepList { previouseSteps = newPrevSteps, currentStep = currStep, nextSteps = newNextSteps })