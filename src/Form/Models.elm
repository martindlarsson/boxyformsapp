module Form.Models exposing (..)


type alias Index =
    Int


type alias FormId =
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
    , formSteps : List FormStep
    }


emptyForm : Form
emptyForm =
    Form 0 "" "" 0 "" []


type alias FormStep =
    { stepTitle : String
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


emptyAnswer : QuestionId -> Answer
emptyAnswer qId =
    Answer qId ""
