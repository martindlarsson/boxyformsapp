module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder)
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
    , userData : Maybe UserData
    }


type alias UserData =
    { createdAt : String
    , updatedAt : String
    , orgName : String
    , stripeAccount : String
    }


emptyUserData : UserData
emptyUserData =
    { createdAt = ""
    , updatedAt = ""
    , orgName = ""
    , stripeAccount = ""
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "id" Decode.string
        |> required "email" Decode.string
        |> required "displayName" Decode.string
        |> required "imageUrl" (Decode.nullable Decode.string)
        |> hardcoded Nothing


encode : User -> Value
encode user =
    Encode.object
        [ "id" => Encode.string user.id
        , "email" => Encode.string user.email
        , "displayName" => Encode.string user.displayName
        , "imageUrl" => EncodeExtra.maybe Encode.string user.imageUrl
        ]


dataDecoder : Decoder UserData
dataDecoder =
    decode UserData
        |> required "createdAt" Decode.string
        |> required "updatedAt" Decode.string
        |> required "orgName" Decode.string
        |> required "stripeAccount" Decode.string


encodeData : UserData -> Value
encodeData userData =
    Encode.object
        [ "createdAt" => Encode.string userData.createdAt
        , "updatedAt" => Encode.string userData.updatedAt
        , "orgName" => Encode.string userData.orgName
        , "stripeAccount" => Encode.string userData.stripeAccount
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
            if (user.userData == Nothing) then
                UserNeedsMoreInfo
            else
                UserIsOK
