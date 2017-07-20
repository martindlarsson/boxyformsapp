module Data.Form exposing (..)

import Json.Decode exposing (int, string, nullable, bool, Decoder, list, andThen, succeed)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Form =
    { id : FormId
    , name : String
    , description : String
    , openDate : String
    , closeDate : String
    , public : Bool
    , imgUrl : String
    , orgName : String
    , orgId : String
    }



-- TODO add Tags list


type alias FormId =
    String


emptyForm : Form
emptyForm =
    Form
        "new form"
        ""
        ""
        ""
        ""
        False
        ""
        ""
        ""


decodeForm : Json.Decode.Value -> Result String Form
decodeForm jsonForm =
    Json.Decode.decodeValue jsonFormDecoder jsonForm


decodeFormList : Json.Decode.Value -> Result String (List Form)
decodeFormList jsonFormList =
    Json.Decode.decodeValue jsonFormListDecoder jsonFormList


jsonFormListDecoder : Decoder (List Form)
jsonFormListDecoder =
    list jsonFormDecoder


jsonFormDecoder : Decoder Form
jsonFormDecoder =
    decode Form
        |> required "id" string
        |> required "name" string
        |> required "description" string
        |> required "openDate" string
        |> required "closeDate" string
        |> required "public" bool
        |> required "imgUrl" string
        |> required "orgName" string
        |> required "orgId" string
