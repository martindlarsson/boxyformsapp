

type alias Form =
    { eventId : EventId
    , eventName : String
    , orgName : String
    , formId : FormId
    , formName : String
    , formSteps : List FormStep
    }

type alias Question =
    { questionText : String
    , questionType : QuestionType
    , questionIndex : Index
    , choices : List Choice
    }
