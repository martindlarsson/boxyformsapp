module Data.User exposing (User, decoder, encode, usernameParser)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import UrlParser


type alias User =
    { id : String
    , email : String
    , displayName : String
    , imageUrl : Maybe String
    , createdAt : String
    , updatedAt : String
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "displayName" Decode.string
        |> required "imageUrl" (Decode.nullable Decode.string)
        |> required "createdAt" Decode.string
        |> required "updatedAt" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ "id" => Encode.string user.id
        , "email" => Encode.string user.email
        , "displayName" => Encode.string user.displayName
        , "imageUrl" => EncodeExtra.maybe Encode.string user.imageUrl
        , "createdAt" => Encode.string user.createdAt
        , "updatedAt" => Encode.string user.updatedAt
        ]


type Username
    = Username String


usernameParser : UrlParser.Parser (String -> a) a
usernameParser =
    UrlParser.custom "USERNAME" Ok
