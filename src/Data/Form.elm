module Data.Form exposing (..)

import Json.Decode exposing (int, string, nullable, bool, Decoder, list, andThen, succeed, map, oneOf)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Data.User exposing (..)
import Util exposing (..)
import Reorderable exposing (Reorderable)


type alias Form =
    { id : String
    , name : String
    , description : String
    , dateFrom : String
    , dateTo : String
    , public : Bool
    , imgUrl : String
    , orgName : String
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


-- Decoder
-- reorderable : Decoder a -> Decoder (Reorderable a)
-- reorderable decoder = list decoder |> Json.Decode.map Reorderable.fromList

-- decodeForm : Json.Decode.Value -> Result String Form
-- decodeForm jsonForm =
--     Json.Decode.decodeValue jsonFormDecoder jsonForm


-- jsonFormDecoder : Decoder Form
-- jsonFormDecoder =
--     decode Form
--         |> required "id" string
--         |> required "name" string
--         |> required "description" string
--         |> required "openDate" string
--         |> required "closeDate" string
--         |> required "public" bool
--         |> required "imgUrl" string
--         |> required "orgName" string        
--         |> required "questions" (reorderable questionDecoder)

-- questionDecoder : Decoder Question
-- questionDecoder =
--     decode Question
--         |> required "questionText" string
--         |> required "questionType" questionTypeDecoder

-- questionTypeDecoder : Decoder QuestionType
-- questionTypeDecoder =
--     oneOf
--         [ questionTextTypeDecoder
--         , questionInfoTypeDecoder
--         , questionYesNoTypeDecoder ]

-- questionTextTypeDecoder : Decoder QuestionType
-- questionTextTypeDecoder =
--     succeed TextType

-- questionInfoTypeDecoder : Decoder QuestionType
-- questionInfoTypeDecoder =
--     succeed InfoType

-- questionChoiceTypeDecoder : Decoder QuestionType
-- questionChoiceTypeDecoder =
--     map
--         (\response -> ChoiceType response.data)
--         choiceListDecoder

-- questionYesNoTypeDecoder : Decoder QuestionType
-- questionYesNoTypeDecoder =
--     succeed YesNoType

-- choiceListDecoder : Decoder (Reorderable Choice)
-- choiceListDecoder =
--     decode (list Choice)
--         |> reorderable choiceDecoder

-- choiceDecoder : Decoder Choice
-- choiceDecoder =
--     decode Choice
--         |> required "choiceText" string



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

