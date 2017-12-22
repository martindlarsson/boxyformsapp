module Main exposing (..)

import Window as Window exposing (..)
import Task exposing (..)
import Route exposing (Route, routeToString)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import BoxyStyle exposing (..)
import Ports exposing (..)
import Element as El exposing (..)
import Element.Keyed as Keyed exposing (..)
import Element.Attributes as Attr exposing (..)
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
                [ Attr.width fill ]
            else if (device.tablet) then
                [ Attr.width (px 600), center ]
            else
                [ Attr.width (px 800), center ]
    in
        El.viewport (boxyStylesheet model.device) <|
            El.grid Main
                [ spacing 20 ]
                { columns = [ fill ]
                , rows = [ px 80, px (toFloat (device.height - 80)) ]
                , cells =
                    [ El.cell
                        { start = ( 0, 0 )
                        , width = 1
                        , height = 1
                        , content = navigation model
                        }
                    , El.cell
                        { start = ( 0, 1 )
                        , width = 1
                        , height = 1
                        , content =
                            el Page
                                [ spacing 20, padding 20, paddingBottom 50 ]
                                (el
                                    None
                                    pageLayout
                                    (viewPage page model)
                                )
                        }
                    ]
                }



-- TODO gör responsiv och dölj saker på mindre skärmar


navigation : Model -> Element Styles variation msg
navigation model =
    let
        activePage =
            model.activePage
    in
        El.row Navigation
            [ spread, paddingXY 80 20, verticalCenter ]
            [ link (routeToString Route.Home) <| el Logo [] (El.text "BoxyForms")
            , El.row None
                [ spacing 20, center, verticalCenter ]
                (signInLink model)
            ]


signInLink : Model -> List (Element Styles variation msg)
signInLink model =
    let
        user =
            model.user

        activeRoute =
            model.activeRoute
    in
        if (user == Nothing) then
            [ link (routeToString Route.Login) <| el (navStyle activeRoute Route.Login) [] (El.text "Logga in") ]
        else
            [ link (routeToString Route.NewForm) <| el (navStyle activeRoute Route.NewForm) [] (El.text "Nytt formulär")
            , link (routeToString Route.MyForms) <| el (navStyle activeRoute Route.MyForms) [] (El.text "Mina formulär")
            , link (routeToString Route.Profile) <| el (navStyle activeRoute Route.Profile) [] (El.text "Min profil")
            , link (routeToString Route.Logout) <| el NavOption [] (El.text "Logga ut")
            ]


viewPage : Page -> Model -> Element Styles variation Msg
viewPage page model =
    case page of
        NotFound ->
            El.text "Sidan inte funnen"

        Home ->
            El.map HomePageMsg (HomePage.view model.user)

        MyForms ->
            El.text "Mina formulär"

        NewForm pageModel ->
            Keyed.row None [] [ ( "new_form", (El.map NewFormPageMsg (NewFormPage.view pageModel)) ) ]

        Profile user ->
            Keyed.row None [] [ ( "profile_page", (El.map ProfilePageMsg (ProfilePage.view user)) ) ]

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

        _ =
            Debug.log "setRoute validateUser" isUserDataOK

        page =
            model.activePage
    in
        case ( msg, page ) of
            ( NoOp, _ ) ->
                ( model, Cmd.none )

            ( WindowResize wSize, _ ) ->
                ( { model | device = Just (classifyDevice wSize) }, Cmd.none )

            ( SetRoute route, _ ) ->
                -- setRoute route model
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
                    ( { model | activePage = NewForm newFormPageModel }, cmd )

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
                    ( { model | activePage = (NewForm (NewFormPage.init logedInUser)), activeRoute = Route.NewForm }, Cmd.none )



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


navStyle : Route -> Route -> Styles
navStyle activeRoute navRoute =
    if (activeRoute == navRoute) then
        ActiveNavOption
    else
        NavOption


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
