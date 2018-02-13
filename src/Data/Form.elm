module Data.Form exposing (..)

-- import Json.Decode exposing (int, string, nullable, bool, Decoder, list, andThen, succeed)
-- import Json.Decode.Pipeline exposing (decode, required, optional)

import Data.User exposing (..)
import Array exposing (..)
import Util exposing (..)


type alias Form =
    { id : String
    , name : String
    , description : String
    , dateFrom : String
    , dateTo : String
    , public : Bool
    , imgUrl : String
    , orgName : String
    , questions : Array Question
    }


type alias QuestionId =
    String


type alias ChoiceId =
    String


type alias Question =
    { id : QuestionId
    , questionText : String
    , questionType : QuestionType
    }


type QuestionType
    = TextType
    | InfoType
    | ChoiceType (List Choice)
    | YesNoType


type alias Choice =
    { id : ChoiceId
    , choiceText : String
    }



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
    , questions = Array.empty
    }


emptyQuestion : QuestionType -> Question
emptyQuestion qType =
    { id = "new" -- toString (Random.int Random.minInt Random.maxInt)
    , questionText = ""
    , questionType = qType
    }


emptyChoice : Choice
emptyChoice =
    { id = "new"
    , choiceText = ""
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
-- getQuestionWitId : Array Question -> QuestionId -> Int -> Maybe ( Question, Int )
-- getQuestionWitId questions id startIndex =
--     case (Array.isEmpty questions) of
--         True ->
--             Nothing
--         False ->
--             let
--                 maybeFirstQuestion =
--                     Array.get 0 questions
--                 arrayLenght =
--                     Array.length questions
--                 restQuestions =
--                     Array.slice 1 arrayLenght questions
--             in
--                 case maybeFirstQuestion of
--                     Nothing ->
--                         Nothing
--                     Just firstQuestion ->
--                         if (firstQuestion.id == id) then
--                             Just ( firstQuestion, startIndex )
--                         else
--                             getQuestionWitId restQuestions id (startIndex + 1)
-- Get --


getQuestionWitId : Form -> QuestionId -> Maybe Question
getQuestionWitId form questionId =
    let
        filteredArray =
            Array.filter (\q -> q.id == questionId) form.questions
    in
        Array.get 0 filteredArray


getQuestionIndex : Form -> QuestionId -> Maybe Int
getQuestionIndex form questionId =
    let
        indexedArray =
            Array.indexedMap (,) form.questions

        filteredArray =
            Array.filter (\( qIndex, question ) -> question.id == questionId) indexedArray

        maybeQuestion =
            Array.get 0 filteredArray
    in
        case maybeQuestion of
            Nothing ->
                Nothing

            Just ( index, question ) ->
                Just index


getChoiceWithId : Question -> ChoiceId -> Maybe Choice
getChoiceWithId question choiceId =
    case question.questionType of
        ChoiceType choiceList ->
            let
                filteredList =
                    List.filter (\c -> c.id == choiceId) choiceList
            in
                List.head filteredList

        _ ->
            Nothing



-- Update --


updateChoice : Form -> QuestionId -> ChoiceId -> Choice -> Form
updateChoice oldForm questionId choiceId newChoice =
    let
        maybeOldQuestion =
            getQuestionWitId oldForm questionId

        oldChoices =
            case maybeOldQuestion of
                Nothing ->
                    []

                Just oldQuestion ->
                    case oldQuestion.questionType of
                        ChoiceType choiceList ->
                            choiceList

                        _ ->
                            []

        oldChoice =
            oldChoices
                |> List.filter (\c -> c.id == choiceId)
                |> List.head

        newChoices =
            let
                update oldChoice =
                    if (oldChoice.id == choiceId) then
                        newChoice
                    else
                        oldChoice
            in
                List.map update oldChoices
    in
        case maybeOldQuestion of
            Nothing ->
                oldForm

            Just oldQuestion ->
                if (List.isEmpty oldChoices) then
                    oldForm
                else
                    updateFormWithQuestion oldForm questionId { oldQuestion | questionType = ChoiceType newChoices }


updateChoiceId : Form -> QuestionId -> ChoiceId -> Int -> Form
updateChoiceId oldForm questionId oldChoiceId newChoiceId =
    let
        maybeOldQuestion =
            getQuestionWitId oldForm questionId
    in
        case maybeOldQuestion of
            Nothing ->
                oldForm

            Just oldQuestion ->
                case oldQuestion.questionType of
                    ChoiceType choiceList ->
                        let
                            updateId choice =
                                if (choice.id == oldChoiceId) then
                                    { choice | id = toString newChoiceId }
                                else
                                    choice

                            newChoices =
                                List.map updateId choiceList

                            newQuestion =
                                { oldQuestion | questionType = ChoiceType newChoices }
                        in
                            updateFormWithQuestion oldForm oldQuestion.id newQuestion

                    _ ->
                        oldForm


updateQuestionId : Form -> QuestionId -> Int -> Form
updateQuestionId oldForm questionId newQuestionId =
    let
        maybeOldQuestion =
            getQuestionWitId oldForm questionId
    in
        case maybeOldQuestion of
            Nothing ->
                oldForm

            Just oldQuestion ->
                let
                    newQuestion =
                        { oldQuestion | id = toString newQuestionId }
                in
                    updateFormWithQuestion oldForm questionId newQuestion


addQuestion : Form -> QuestionType -> Int -> Form
addQuestion oldForm qType index =
    let
        questionToInsert =
            emptyQuestion qType

        newQuestions =
            insertItemIntoArray questionToInsert index oldForm.questions
    in
        { oldForm | questions = newQuestions }


removeQuestion : Form -> QuestionId -> Form
removeQuestion oldForm questionId =
    let
        newQuestions =
            Array.filter (\q -> q.id /= questionId) oldForm.questions
    in
        { oldForm | questions = newQuestions }


updateQuestion : Array Question -> QuestionId -> Question -> Array Question
updateQuestion oldQuestions questionId newQuestion =
    let
        update oldQuestion =
            if (oldQuestion.id == questionId) then
                newQuestion
            else
                oldQuestion
    in
        Array.map update oldQuestions


updateFormWithQuestion : Form -> QuestionId -> Question -> Form
updateFormWithQuestion oldForm questionId newQuestion =
    { oldForm | questions = updateQuestion oldForm.questions questionId newQuestion }
