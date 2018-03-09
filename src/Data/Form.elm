module Data.Form exposing (..)

import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JE exposing (..)
import Data.User exposing (..)
import Reorderable exposing (Reorderable)


type alias FormId = String

type alias Form =
    { id : FormId
    , name : String
    , description : String
    , dateFrom : String
    , dateTo : String
    , public : Bool
    , imgUrl : String
    , orgName : String
    , userId : String
    , questions : Reorderable Question
    }


type alias Question =
    { questionText : String
    , questionType : QuestionType
    }


type QuestionType
    = TextType
    | InfoType
    | ChoiceType (Reorderable Choice)
    | YesNoType


type alias Choice =
    { choiceText : String
    }

type alias QuestionIdx =
    Int

type alias ChoiceIdx =
    Int


emptyForm : User -> Form
emptyForm user =
    { id = "new form"
    , name = ""
    , description = ""
    , dateFrom = ""
    , dateTo = ""
    , public = False
    , imgUrl = ""
    , orgName = Maybe.withDefault "" user.orgName
    , userId = user.id
    , questions = Reorderable.empty
    }


emptyQuestion : QuestionType -> Question
emptyQuestion qType =
    { questionText = ""
    , questionType = qType
    }


emptyChoice : Choice
emptyChoice =
    { choiceText = ""
    }

-- Encoder
encodeForm : Form -> JE.Value
encodeForm form =
    JE.object
        [ ("id", JE.string form.id)
        , ("name", JE.string form.name)
        , ("description", JE.string form.description)
        , ("dateFrom", JE.string form.dateFrom)
        , ("dateTo", JE.string form.dateTo)
        , ("public", JE.bool form.public)
        , ("imgUrl", JE.string form.imgUrl)
        , ("orgName", JE.string form.orgName)
        , ("userId", JE.string form.userId)
        , ("questions", JE.list (encodeQuestions form.questions))
        ]

encodeQuestions : Reorderable Question -> List JE.Value
encodeQuestions questions =
    List.map encodeQuestion (Reorderable.toList questions)

encodeQuestion : Question -> JE.Value
encodeQuestion question =
    JE.object
        [ ("questionText", JE.string question.questionText)
        , ("questionType", encodeQuestionType question.questionType)
        ]

encodeQuestionType : QuestionType -> JE.Value
encodeQuestionType questionType =
    case questionType of
      TextType -> JE.object [ ( "type", JE.string "text" ) ]
      InfoType -> JE.object [ ( "type", JE.string "info") ]
      ChoiceType choices -> JE.object [ ( "type", JE.string "choice" ), ( "choices", JE.list (encodeChoices choices) ) ]
      YesNoType -> JE.object [ ( "type", JE.string "yesno") ]


encodeChoices : Reorderable Choice -> List JE.Value
encodeChoices choices =
    List.map encodeChoice (Reorderable.toList choices)

encodeChoice : Choice -> JE.Value
encodeChoice choice =
    JE.object [ ( "choiceText", JE.string choice.choiceText ) ]


-- Decoder
reorderable : Decoder a -> Decoder (Reorderable a)
reorderable decoder = JD.list decoder |> JD.map Reorderable.fromList

decodeForm : JD.Value -> Result String Form
decodeForm jsonForm =
    JD.decodeValue formDecoder jsonForm


formDecoder : Decoder Form
formDecoder =
    decode Form
        |> required "id" JD.string
        |> required "name" JD.string
        |> required "description" JD.string
        |> required "openDate" JD.string
        |> required "closeDate" JD.string
        |> required "public" JD.bool
        |> required "imgUrl" JD.string
        |> required "orgName" JD.string
        |> required "userId" JD.string         
        |> required "questions" (reorderable questionDecoder)

questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "questionText" JD.string
        |> required "questionType" questionTypeDecoder

questionTypeDecoder : Decoder QuestionType
questionTypeDecoder =
    field "type" JD.string
    |> andThen (\typeString ->
      case typeString of
        "text" -> succeed TextType
        "info" -> succeed InfoType
        "choice" -> map ChoiceType (field "choices" choiceListDecoder)
        "yesno" -> succeed YesNoType
        _ -> fail "I don't know how to decode that type yet"
    )

choiceListDecoder : Decoder (Reorderable Choice)
choiceListDecoder =
    reorderable choiceDecoder

choiceDecoder : Decoder Choice
choiceDecoder =
    decode Choice
        |> required "choiceText" JD.string



-- Helper functions

-- Get --

getIndexedList : Reorderable a -> List (Int, a)
getIndexedList oldList =
    List.indexedMap (,) 
    <| Reorderable.toList oldList

getItem : Reorderable a -> Int -> Maybe a
getItem oldList index =
    Reorderable.get index oldList


-- Update --


updateChoice : Reorderable Choice -> Int -> String -> Reorderable Choice
updateChoice oldChoices index newValue =
    let
        updateField oldChoice =
            { oldChoice | choiceText = newValue }
    in
        Reorderable.update index updateField oldChoices


addQuestion : Reorderable Question -> QuestionType -> Int -> Reorderable Question
addQuestion oldQuestions qType index =
    let
        questionToInsert = emptyQuestion qType
    in
        Reorderable.insertAt index questionToInsert oldQuestions


updateQuestion : Reorderable Question -> Int -> (Question -> Question) -> Reorderable Question
updateQuestion oldQuestions index newQuestionFunc =
    Reorderable.update index newQuestionFunc oldQuestions


updateFormWithQuestion : Form -> Int -> Question -> Form
updateFormWithQuestion oldForm index newQuestion =
    let
        updateQuestion_int oldQuestion = newQuestion
    in
        { oldForm | questions = updateQuestion oldForm.questions index updateQuestion_int }

