module Form.FormDecoder exposing(..)

import Form.Models exposing(..)
import Json.Decode exposing (int, string, float, nullable, Decoder, list, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import List exposing(head)

decodeForm : List Json.Decode.Value -> Result String Form
decodeForm modelJson =
    Json.Decode.decodeValue formDecoder ( head modelJson )

formDecoder : Decoder Form
formDecoder =
    decode Form
        |> required "eventId" int
        |> required "eventName" string
        |> required "orgName" string
        |> required "formId" int
        |> required "formName" string
        |> required "formSteps" (list formStepDecoder)

formStepDecoder : Decoder FormStep
formStepDecoder =
    decode FormStep
        |> required "stepTitle" string
        |> required "stepIndex" int
        |> required "questions" (list questionDecoder)

questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "questionId" int -- Nödvändigt?
        |> required "questionText" string
        |> required "questionType" (string |> andThen stringToQuestionType)
        |> required "questionIndex" int
        |> required "choices" (nullable (list choiceDecoder))


stringToQuestionType : String -> QuestionType
stringToQuestionType typeString =
    -- typeString
        case typeString of
        "textType" -> TextType
        "textType_email" -> TextType_email
        "choiceType" -> ChoiceType
        "infoType" -> InfoType
        

choiceDecoder : Decoder Choice
choiceDecoder =
    decode Choice -- choiceId ?
        |> required "choiceFee" int
        |> required "choiceText" string
        |> required "choiceIndex" int