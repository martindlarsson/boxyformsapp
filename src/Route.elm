module Route exposing (Route(..), fromLocation, routeToString, modifyUrl)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Data.Form exposing (..)


-- ROUTING --


type Route
    = Home
    | Login
    | Logout
    | Profile
    | NewForm
    | EditForm FormId



-- | Register
-- | Settings


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map NewForm (s "newform")
        , Url.map EditForm (s "editform" </> string)
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

                NewForm ->
                    [ "newform" ]

                EditForm formId ->
                    [ "editform", formId ]

                Profile ->
                    [ "profile" ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
