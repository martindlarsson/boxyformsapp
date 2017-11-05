module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, hardcoded)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import UrlParser


type alias User =
    { id : String
    , email : String
    , displayName : String
    , imageUrl : Maybe String
    , createdAt : Maybe String
    , updatedAt : Maybe String
    , orgName : Maybe String
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "displayName" Decode.string
        |> required "imageUrl" (Decode.nullable Decode.string)
        |> required "createdAt" (Decode.nullable Decode.string)
        |> required "updatedAt" (Decode.nullable Decode.string)
        |> required "orgName" (Decode.nullable Decode.string)


encode : User -> Value
encode user =
    Encode.object
        [ "id" => Encode.string user.id
        , "email" => Encode.string user.email
        , "displayName" => Encode.string user.displayName
        , "imageUrl" => EncodeExtra.maybe Encode.string user.imageUrl
        , "createdAt" => EncodeExtra.maybe Encode.string user.createdAt
        , "updatedAt" => EncodeExtra.maybe Encode.string user.updatedAt
        , "orgName" => EncodeExtra.maybe Encode.string user.orgName
        ]


type Username
    = Username String


usernameParser : UrlParser.Parser (String -> a) a
usernameParser =
    UrlParser.custom "USERNAME" Ok


type UserState
    = UserIsOK
    | UserNeedsMoreInfo
    | NotLoggedIn


validateUser : Maybe User -> UserState
validateUser user =
    case user of
        Nothing ->
            NotLoggedIn

        Just user ->
            if (user.orgName == Nothing) then
                UserNeedsMoreInfo
            else
                UserIsOK
