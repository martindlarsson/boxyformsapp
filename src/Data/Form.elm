module Data.Form exposing (..)

-- import Json.Decode exposing (int, string, nullable, bool, Decoder, list, andThen, succeed)
-- import Json.Decode.Pipeline exposing (decode, required, optional)

import Data.User exposing (..)
import Array exposing (..)


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
    { id : String
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


getItemWitId : Array Question -> QuestionId -> Int -> Maybe ( Question, Int )
getItemWitId questions id startIndex =
    case (Array.isEmpty questions) of
        True ->
            Nothing

        False ->
            let
                maybeFirstQuestion =
                    Array.get 0 questions

                arrayLenght =
                    Array.length questions

                restQuestions =
                    Array.slice 1 arrayLenght questions
            in
                case maybeFirstQuestion of
                    Nothing ->
                        Nothing

                    Just firstQuestion ->
                        if (firstQuestion.id == id) then
                            Just ( firstQuestion, startIndex )
                        else
                            getItemWitId restQuestions id (startIndex + 1)


getItemIndex_Internal : Array Question -> Question -> Int -> Maybe Int
getItemIndex_Internal questions question startIndex =
    let
        maybeQuestion =
            getItemWitId questions question.id startIndex
    in
        case maybeQuestion of
            Nothing ->
                Nothing

            Just ( question, questionIndex ) ->
                Just questionIndex


getItemIndex : Array Question -> Question -> Maybe Int
getItemIndex questions question =
    getItemIndex_Internal questions question 0
