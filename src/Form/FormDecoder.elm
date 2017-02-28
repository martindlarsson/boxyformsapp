module FormDecoder exposing (..)

decodeForm : Json.Decode.Value -> Result String (List Form)
decodeForm modelJson =
    Json.Decode.decodeValue formDecoder modelJson

formDecoder : Decoder Form
formDecoder =
    decode Form
        |> required "eventId" int
        |> required "eventName" string
        |> required "orgName" string
        |> required "formId" int
        |> required "formName" string
        |> required "formSteps" (list formStepDecoder)

questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "questionId" int -- Nödvändigt?
        |> required "questionText" string
        |> required "questionType" (string |> andThen stringToQuestionType)
        |> required "questionIndex" int
        |> optional "choices" (list choiceDecoder) []

choiceDecoder : Decoder Choice
choiceDecoder =
    decode Choice -- choiceId ?
        |> required "choiceFee" int
        |> required "choiceText" string
        |> required "choiceIndex" int