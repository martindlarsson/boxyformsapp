module Data.Form exposing (..)

import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as JE exposing (..)
import Data.User exposing (..)
import Reorderable as R exposing (Reorderable)


-- import UrlParser


type alias FormId =
    String


type alias Form =
    { id : String
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
    , required : Bool
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
    , questions = R.empty
    }


emptyQuestion : QuestionType -> Question
emptyQuestion qType =
    { questionText = ""
    , questionType = qType
    , required = False
    }


emptyChoice : Choice
emptyChoice =
    { choiceText = ""
    }



-- Url parsers
-- formIdToString : Form -> String
-- formIdToString form =
--     let
--         (FormId formId) =
--             form.id
--     in
--         formId
-- formIdParser : Form -> UrlParser.Parser (FormId -> a) a
-- formIdParser form =
--     -- let
--     --     -- parseFormId = \form = formIdToString form
--     --     formIdResult = (\form -> Ok <| formIdToString form.id)
--     -- in
--     UrlParser.custom "FORMID" (\form -> Ok <| formIdToString form.id)
--(Ok << parseFormId)
-- Encoder


encodeForm : Form -> JE.Value
encodeForm form =
    JE.object
        [ ( "id", JE.string form.id )
        , ( "name", JE.string form.name )
        , ( "description", JE.string form.description )
        , ( "dateFrom", JE.string form.dateFrom )
        , ( "dateTo", JE.string form.dateTo )
        , ( "public", JE.bool form.public )
        , ( "imgUrl", JE.string form.imgUrl )
        , ( "orgName", JE.string form.orgName )
        , ( "userId", JE.string form.userId )
        , ( "questions", JE.list (encodeQuestions form.questions) )
        ]



-- encodeFormId : FormId -> JE.Value
-- encodeFormId (FormId formId) =
--     JE.string formId


encodeQuestions : Reorderable Question -> List JE.Value
encodeQuestions questions =
    List.map encodeQuestion (R.toList questions)


encodeQuestion : Question -> JE.Value
encodeQuestion question =
    JE.object
        [ ( "questionText", JE.string question.questionText )
        , ( "questionType", encodeQuestionType question.questionType )
        , ( "required", JE.bool question.required )
        ]


encodeQuestionType : QuestionType -> JE.Value
encodeQuestionType questionType =
    case questionType of
        TextType ->
            JE.object [ ( "type", JE.string "text" ) ]

        InfoType ->
            JE.object [ ( "type", JE.string "info" ) ]

        ChoiceType choices ->
            JE.object [ ( "type", JE.string "choice" ), ( "choices", JE.list (encodeChoices choices) ) ]

        YesNoType ->
            JE.object [ ( "type", JE.string "yesno" ) ]


encodeChoices : Reorderable Choice -> List JE.Value
encodeChoices choices =
    List.map encodeChoice (R.toList choices)


encodeChoice : Choice -> JE.Value
encodeChoice choice =
    JE.object [ ( "choiceText", JE.string choice.choiceText ) ]



-- Decoder


reorderable : Decoder a -> Decoder (Reorderable a)
reorderable decoder =
    JD.list decoder |> JD.map R.fromList


decodeForm : JD.Value -> Result String Form
decodeForm jsonForm =
    JD.decodeValue formDecoder jsonForm


decodeForms : JD.Value -> Result String (List Form)
decodeForms jsonForms =
    JD.decodeValue (JD.list formDecoder) jsonForms


formDecoder : Decoder Form
formDecoder =
    decode Form
        |> required "id" JD.string
        |> required "name" JD.string
        |> required "description" JD.string
        |> required "dateFrom" JD.string
        |> required "dateTo" JD.string
        |> required "public" JD.bool
        |> required "imgUrl" JD.string
        |> required "orgName" JD.string
        |> required "userId" JD.string
        |> required "questions" (reorderable questionDecoder)



-- formIdDecoder : Decoder FormId
-- formIdDecoder =
--     JD.map FormId JD.string


questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "questionText" JD.string
        |> required "questionType" questionTypeDecoder
        |> required "required" JD.bool


questionTypeDecoder : Decoder QuestionType
questionTypeDecoder =
    field "type" JD.string
        |> andThen
            (\typeString ->
                case typeString of
                    "text" ->
                        succeed TextType

                    "info" ->
                        succeed InfoType

                    "choice" ->
                        map ChoiceType (field "choices" choiceListDecoder)

                    "yesno" ->
                        succeed YesNoType

                    _ ->
                        fail "I don't know how to decode that type yet"
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


getIndexedList : Reorderable a -> List ( Int, a )
getIndexedList oldList =
    List.indexedMap (,) <|
        R.toList oldList


getItem : Reorderable a -> Int -> Maybe a
getItem oldList index =
    R.get index oldList


getListItem : List Form -> FormId -> Maybe Form
getListItem forms formId =
    List.head <| List.filter (\form -> form.id == formId) forms



-- Update --


updateListItem : List Form -> FormId -> Form -> List Form
updateListItem oldList newFormId newForm =
    let
        _ =
            Debug.log "updateListItem" newFormId

        reorderableList =
            R.fromList oldList

        keyedList =
            R.toKeyedList reorderableList

        maybeKeyedItem =
            List.filter (\( key, form ) -> form.id == newFormId) keyedList |> List.head
    in
        case maybeKeyedItem of
            Nothing ->
                List.append oldList [ newForm ]

            Just ( key, oldForm ) ->
                let
                    keyResult =
                        String.toInt key
                in
                    case keyResult of
                        Ok key ->
                            R.update key (\_ -> newForm) reorderableList |> R.toList

                        Err msg ->
                            oldList



-- let
--     updateForm // TODO, gör en funktion som byter ut ett befintlig formulär mot ett nytt
-- in


updateChoice : Reorderable Choice -> Int -> String -> Reorderable Choice
updateChoice oldChoices index newValue =
    let
        updateField oldChoice =
            { oldChoice | choiceText = newValue }
    in
        R.update index updateField oldChoices


addQuestion : Reorderable Question -> QuestionType -> Int -> Reorderable Question
addQuestion oldQuestions qType index =
    let
        questionToInsert =
            emptyQuestion qType
    in
        R.insertAt index questionToInsert oldQuestions


updateQuestion : Reorderable Question -> Int -> (Question -> Question) -> Reorderable Question
updateQuestion oldQuestions index newQuestionFunc =
    R.update index newQuestionFunc oldQuestions


updateFormWithQuestion : Form -> Int -> Question -> Form
updateFormWithQuestion oldForm index newQuestion =
    let
        updateQuestion_int oldQuestion =
            newQuestion
    in
        { oldForm | questions = updateQuestion oldForm.questions index updateQuestion_int }
