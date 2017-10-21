module Route exposing (Route(..), fromLocation, href, modifyUrl, routeToString)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Data.User as User exposing (..)


-- ROUTING --


type Route
    = Home
    | Login
    | Logout
    | MyForms
    | Profile
    | NewForm



-- | Register
-- | Settings


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map MyForms (s "myforms")
        , Url.map NewForm (s "newform")
        , Url.map Profile (s "profile")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                MyForms ->
                    [ "myforms" ]

                NewForm ->
                    [ "newform" ]

                Profile ->
                    [ "profile" ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
