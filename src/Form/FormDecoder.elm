module Form.FormDecoder exposing(..)

import Form.Models exposing(..)
import Json.Decode exposing (int, string, nullable, Decoder, list, andThen, succeed)
import Json.Decode.Pipeline exposing (decode, required, optional)

-- decodeForm : List Json.Decode.Value -> Result String Form
-- decodeForm modelJson =
--     let
--         firstFormInList = ( head modelJson )
--     in
--         case firstFormInList of
--             Just firstForm ->
--                 Json.Decode.decodeValue formDecoder firstForm
            
--             Nothing ->
--                 Err "No forms in JSON string"

-- decodeForm : Json.Decode.Value -> Result String (List Form)
-- decodeForm modelJson =
--     Json.Decode.decodeValue (list formDecoder) modelJson


decodeForm : Json.Decode.Value -> Result String (List Form)
decodeForm modelJson =
    Json.Decode.decodeValue decodeFormList modelJson

decodeFormList : Decoder (List Form)
decodeFormList =
    list formDecoder


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
        |> required "questionType" (string |> andThen questionTypeDecoder)
        |> required "questionIndex" int
        |> optional "choices" (list choiceDecoder) []
        -- |> required "choices" (nullable (list choiceDecoder))
        

questionTypeDecoder : String -> Decoder QuestionType
questionTypeDecoder typeString =
    case typeString of
        "textType" -> Json.Decode.succeed TextType
        "textType_email" -> Json.Decode.succeed TextType_email
        "choiceType" -> Json.Decode.succeed ChoiceType
        "infoType" -> Json.Decode.succeed InfoType
        _ -> Json.Decode.succeed NoType
        

choiceDecoder : Decoder Choice
choiceDecoder =
    decode Choice -- choiceId ?
        |> required "choiceFee" int
        |> required "choiceIndex" int
        |> required "choiceText" string