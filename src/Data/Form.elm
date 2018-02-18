module Data.Form exposing (..)

-- import Json.Decode exposing (int, string, nullable, bool, Decoder, list, andThen, succeed)
-- import Json.Decode.Pipeline exposing (decode, required, optional)

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


-- TODO add lista med taggar (vilken typ av event)


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



-- decodeForm : Json.Decode.Value -> Result String Form
-- decodeForm jsonForm =
--     Json.Decode.decodeValue jsonFormDecoder jsonForm
-- decodeFormList : Json.Decode.Value -> Result String (List Form)
-- decodeFormList jsonFormList =
--     Json.Decode.decodeValue jsonFormListDecoder jsonFormList
-- jsonFormListDecoder : Decoder (List Form)
-- jsonFormListDecoder =
--     list jsonFormDecoder
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
--         |> required "questions" (List Question)
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


addQuestion : Form -> QuestionType -> Int -> Form
addQuestion oldForm qType index =
    let
        questionToInsert = emptyQuestion qType

        newQuestions = Reorderable.insertAt index questionToInsert oldForm.questions
    in
        { oldForm | questions = newQuestions }


updateQuestion : Reorderable Question -> Int -> (Question -> Question) -> Reorderable Question
updateQuestion oldQuestions index newQuestionFunc =
    Reorderable.update index newQuestionFunc oldQuestions


updateFormWithQuestion : Form -> Int -> Question -> Form
updateFormWithQuestion oldForm index newQuestion =
    let
        updateQuestion_int oldQuestion = newQuestion
    in
        { oldForm | questions = updateQuestion oldForm.questions index updateQuestion_int }


-- General Reorderable

moveItem : Reorderable a -> Int -> MoveOperation -> Reorderable a
moveItem oldList index moveOp =
    case moveOp of
        MoveUp ->
            Reorderable.moveUp index oldList
        MoveDown ->
            Reorderable.moveDown index oldList

addItem : Reorderable a -> a -> Reorderable a
addItem oldList item =
    Reorderable.push item oldList


removeItem : Reorderable a -> Int -> Reorderable a
removeItem oldList index =
    Reorderable.drop index oldList