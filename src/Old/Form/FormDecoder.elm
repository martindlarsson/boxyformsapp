module Form.FormDecoder exposing (..)

import Form.Models exposing (..)
import Json.Decode exposing (int, string, nullable, Decoder, list, andThen, succeed)
import Json.Decode.Pipeline exposing (decode, required, optional)


decodeForm : Json.Decode.Value -> Result String JsonForm
decodeForm modelJson =
    Json.Decode.decodeValue formDecoder modelJson


decodeFormList : Decoder (List JsonForm)
decodeFormList =
    list formDecoder


formDecoder : Decoder JsonForm
formDecoder =
    decode JsonForm
        |> required "eventId" int
        |> required "eventName" string
        |> required "orgName" string
        |> required "formName" string
        |> required "formSteps" (list formStepDecoder)


formStepDecoder : Decoder FormStep
formStepDecoder =
    decode FormStep
        |> required "stepId" string
        |> required "stepTitle" string
        |> required "stepIndex" int
        |> required "questions" (list questionDecoder)


questionDecoder : Decoder Question
questionDecoder =
    decode Question
        -- Nödvändigt?
        |> required "questionId" string
        |> required "questionText" string
        |> required "questionType" (string |> andThen questionTypeDecoder)
        |> required "questionIndex" int
        |> optional "choices" (list choiceDecoder) []


questionTypeDecoder : String -> Decoder QuestionType
questionTypeDecoder typeString =
    case typeString of
        "TextType" ->
            Json.Decode.succeed TextType

        "TextType_email" ->
            Json.Decode.succeed TextType_email

        "ChoiceType" ->
            Json.Decode.succeed ChoiceType

        "InfoType" ->
            Json.Decode.succeed InfoType

        _ ->
            Json.Decode.succeed NoType


choiceDecoder : Decoder Choice
choiceDecoder =
    decode Choice
        |> required "choiceFee" int
        |> required "choiceIndex" int
        |> required "choiceText" string