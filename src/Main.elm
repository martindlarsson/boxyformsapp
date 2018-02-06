module Main exposing (..)

import Window as Window exposing (..)
import Task exposing (..)
import Route exposing (Route, routeToString)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import Ports exposing (..)
import Element as El exposing (..)
import Element.Keyed as Keyed exposing (..)
import Element.Border as Border
import Element.Background as Background
import Color
import Element.Font as Font
import Util exposing (..)
import Html exposing (..)
import Data.User as User exposing (..)
import Page.Login as LoginPage exposing (view)
import Page.Home as HomePage exposing (view, update, Msg)
import Page.Profile as ProfilePage exposing (view, update, Msg)
import Page.NewForm as NewFormPage exposing (view, update, Msg)


type alias Model =
    { activePage : Page
    , activeRoute : Route
    , user : Maybe User
    , device : Maybe Device -- size of the window classified
    }


type Page
    = NotFound
    | Home
    | NewForm NewFormPage.Model
    | MyForms
    | Login
    | Profile User


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    ( { activePage = Home
      , activeRoute = Route.Home
      , user = Nothing
      , device = Nothing
      }
    , Task.perform WindowResize Window.size
    )


view : Model -> Html Msg
view model =
    let
        page =
            model.activePage

        device =
            getDevice model.device

        pageLayout =
            if (device.phone) then
                [ El.width fill ]
            else if (device.tablet) then
                [ El.width (px 600), center ]
            else
                [ El.width (px 800), center ]
    in
        El.layout
            [ Font.family [ Font.sansSerif ]
            , Font.size 14
            , Font.color Color.charcoal
            , Background.color Color.lightGray
            ]
        <|
            El.column
                []
                [ El.row
                    [ El.height (px 80)
                    , Background.color Color.lightOrange
                    , Border.shadow
                        { offset = ( 3, 0 )
                        , blur = 2
                        , size = 3
                        , color = Color.darkGray
                        }
                    ]
                    [ navigation model ]
                , El.row
                    [ El.height <| px (device.height - 80)
                    , padding 20
                    ]
                    [ (viewPage page model) ]
                ]


navigation : Model -> Element msg
navigation model =
    let
        activePage =
            model.activePage
    in
        El.row
            [ centerY
            , center
            , padding 20
            ]
            [ link []
                { url = (routeToString Route.Home)
                , label = (El.el [ Font.size 25 ] (El.text "BoxyForms"))
                }
            , El.row [ El.width fill ] [ El.empty ]
            , El.row
                [ spacing 20, alignRight ]
                (signInLink model)
            ]


signInLink : Model -> List (Element msg)
signInLink model =
    let
        user =
            model.user

        activeRoute =
            model.activeRoute
    in
        if (user == Nothing) then
            [ link [] { url = (routeToString Route.Login), label = (El.el (navStyle activeRoute Route.Login) (El.text "Logga in")) } ]
        else
            [ link [] { url = (routeToString Route.NewForm), label = (El.el (navStyle activeRoute Route.NewForm) (El.text "Nytt formulär")) }
            , link [] { url = (routeToString Route.MyForms), label = (El.el (navStyle activeRoute Route.MyForms) (El.text "Mina formulär")) }
            , link [] { url = (routeToString Route.Profile), label = (El.el (navStyle activeRoute Route.Profile) (El.text "Min profil")) }
            , link [] { url = (routeToString Route.Logout), label = (El.el [ Font.size 16 ] (El.text "Logga ut")) }
            ]


viewPage : Page -> Model -> Element Msg
viewPage page model =
    case page of
        NotFound ->
            El.text "Sidan inte funnen"

        Home ->
            El.map HomePageMsg (HomePage.view model.user)

        MyForms ->
            El.text "Mina formulär"

        NewForm pageModel ->
            Keyed.row [] [ ( "new_form", (El.map NewFormPageMsg (NewFormPage.view pageModel)) ) ]

        Profile user ->
            Keyed.row [] [ ( "profile_page", (El.map ProfilePageMsg (ProfilePage.view user)) ) ]

        Login ->
            LoginPage.view



-- UPDATE --


type Msg
    = NoOp
    | WindowResize Window.Size
    | SetRoute (Maybe Route)
    | UserLoggedIn Value
    | UserLoggedOut
    | ProfilePageMsg ProfilePage.Msg
    | HomePageMsg HomePage.Msg
    | NewFormPageMsg NewFormPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update" msg

        isUserDataOK =
            validateUser model.user

        page =
            model.activePage
    in
        case ( msg, page ) of
            ( NoOp, _ ) ->
                ( model, Cmd.none )

            ( WindowResize wSize, _ ) ->
                ( { model | device = Just (classifyDevice wSize) }, Cmd.none )

            ( SetRoute route, _ ) ->
                case isUserDataOK of
                    UserIsOK ->
                        setRoute route model

                    UserNeedsMoreInfo ->
                        if (route == (Just Route.Home) || route == (Just Route.Logout)) then
                            setRoute route model
                        else
                            setRoute (Just Route.Profile) model

                    NotLoggedIn ->
                        if (route == (Just Route.Home)) then
                            setRoute route model
                        else
                            setRoute (Just Route.Login) model

            -- En användare har loggat in eller skapats
            ( UserLoggedIn value, _ ) ->
                let
                    loggedInUserResult =
                        Decode.decodeValue User.decoder value

                    loggedInUser =
                        case loggedInUserResult of
                            Ok user ->
                                Just user

                            Err str ->
                                Nothing

                    newModel =
                        { model | user = loggedInUser }

                    isUserDataOK =
                        validateUser loggedInUser
                in
                    case isUserDataOK of
                        UserIsOK ->
                            setRoute (Just Route.Home) newModel

                        UserNeedsMoreInfo ->
                            setRoute (Just Route.Profile) newModel

                        NotLoggedIn ->
                            setRoute (Just Route.Login) newModel

            ( UserLoggedOut, _ ) ->
                let
                    updatedModel =
                        { model | user = Nothing }
                in
                    setRoute (Just Route.Home) updatedModel

            ( ProfilePageMsg subMsg, Profile subModel ) ->
                let
                    ( newUser, cmd ) =
                        ProfilePage.update subMsg subModel
                in
                    ( { model | user = Just newUser }, cmd )

            ( HomePageMsg subMsg, _ ) ->
                update (SetRoute (Just Route.NewForm)) model

            ( NewFormPageMsg subMsg, NewForm subModel ) ->
                let
                    ( newFormPageModel, cmd ) =
                        NewFormPage.update subMsg subModel
                in
                    ( { model | activePage = NewForm newFormPageModel }, Cmd.map NewFormPageMsg cmd )

            ( _, _ ) ->
                -- Throw away any stray messages
                ( model, Cmd.none )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | activePage = NotFound, activeRoute = Route.Home }, Cmd.none )

        Just Route.Home ->
            ( { model | activePage = Home, activeRoute = Route.Home }, Cmd.none )

        Just Route.Login ->
            ( { model | activePage = Login, activeRoute = Route.Login }, Ports.startAuthUI () )

        Just Route.Logout ->
            ( { model | activePage = Home, activeRoute = Route.Home }, Ports.logOut () )

        Just Route.MyForms ->
            ( { model | activePage = MyForms, activeRoute = Route.MyForms }, Cmd.none )

        Just Route.Profile ->
            case model.user of
                Nothing ->
                    -- TODO set error message!!
                    ( { model | activePage = Home, activeRoute = Route.Home }, Cmd.none )

                Just logedInUser ->
                    ( { model | activePage = (Profile logedInUser), activeRoute = Route.Profile }, Cmd.none )

        Just Route.NewForm ->
            case model.user of
                Nothing ->
                    -- TODO set error message!!
                    ( { model | activePage = Home, activeRoute = Route.Home }, Cmd.none )

                Just logedInUser ->
                    let
                        device =
                            case model.device of
                                Nothing ->
                                    initialDevice

                                Just currentDevice ->
                                    currentDevice
                    in
                        ( { model | activePage = (NewForm (NewFormPage.init logedInUser device)), activeRoute = Route.NewForm }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.userLoggedIn UserLoggedIn
        , Ports.userLoggedOut (\_ -> UserLoggedOut)
        , Window.resizes (\wSize -> WindowResize wSize)
        ]



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- HELPERS --


navStyle : Route -> Route -> List (El.Attribute msg)
navStyle activeRoute navRoute =
    if (activeRoute == navRoute) then
        [ Font.size 16
        , Font.underline
        , Font.bold
        ]
    else
        [ Font.size 16 ]


getDevice : Maybe Device -> Device
getDevice device =
    case device of
        Nothing ->
            initialDevice

        Just device ->
            device


pageToString : Page -> String
pageToString page =
    case page of
        NotFound ->
            "Notfound"

        Home ->
            "Hem"

        NewForm model ->
            "Nytt formulär"

        MyForms ->
            "Mina formulär"

        Login ->
            "Logga in"

        Profile mode ->
            "Min profil"
