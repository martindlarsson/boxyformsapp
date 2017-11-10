module Main exposing (..)

import Window as Window exposing (..)
import Task exposing (..)
import Route exposing (Route, routeToString)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import BoxyStyle exposing (..)
import Ports exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (..)
import Data.User as User exposing (..)
import Page.Login as LoginPage exposing (view)
import Page.Home as HomePage exposing (view, update, Msg)
import Page.Profile as ProfilePage exposing (view, update, Msg)
import Page.NewForm as NewFormPage exposing (view, update, Msg)


type alias Model =
    { activePage : Page
    , user : Maybe User
    , device : Maybe Device -- size of the window classified
    }


type Page
    = NotFound
    | Home
    | NewForm
    | MyForms
    | Login
    | Profile


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    ( { activePage = Home
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
    in
        Element.viewport (boxyStylesheet model.device) <|
            grid Main
                [ spacing 20 ]
                { columns = [ percent 100 ]
                , rows = [ px 80, px (toFloat (device.height - 80)) ]
                , cells =
                    [ cell
                        { start = ( 0, 0 )
                        , width = 1
                        , height = 1
                        , content = navigation model
                        }
                    , cell
                        { start = ( 0, 1 )
                        , width = 1
                        , height = 1
                        , content =
                            el Page
                                [ spacing 20, padding 20, paddingBottom 50 ]
                                (viewPage page model)
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
        row Navigation
            [ spread, paddingXY 80 20 ]
            [ link (routeToString Route.Home) <| el Logo [] (Element.text "BoxyForms")
            , row None
                [ spacing 20 ]
                (signInLink model)
            ]


signInLink : Model -> List (Element Styles variation msg)
signInLink model =
    let
        user =
            model.user

        activePage =
            model.activePage
    in
        if (user == Nothing) then
            [ link (routeToString Route.Login) <| el (navStyle activePage Login) [] (Element.text "Logga in") ]
        else
            [ link (routeToString Route.NewForm) <| el (navStyle activePage NewForm) [] (Element.text "Nytt formulär")
            , link (routeToString Route.MyForms) <| el (navStyle activePage MyForms) [] (Element.text "Mina formulär")
            , link (routeToString Route.Profile) <| el (navStyle activePage Profile) [] (Element.text "Min profil")
            , link (routeToString Route.Logout) <| el NavOption [] (Element.text "Logga ut")
            ]


viewPage : Page -> Model -> Element Styles variation Msg
viewPage page model =
    case page of
        NotFound ->
            Element.text "Sidan inte funnen"

        Home ->
            Element.map HomePageMsg (HomePage.view model.user)

        MyForms ->
            Element.text "Mina formulär"

        NewForm ->
            Element.map NewFormPageMsg (NewFormPage.view model.user)

        Profile ->
            let
                user =
                    model.user
            in
                case user of
                    Nothing ->
                        Element.text "ERROR!! det finns ingen inloggad användare..."

                    Just user ->
                        Element.map ProfilePageMsg (ProfilePage.view user)

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
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            WindowResize wSize ->
                ( { model | device = Just (classifyDevice wSize) }, Cmd.none )

            SetRoute route ->
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
            UserLoggedIn value ->
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

            UserLoggedOut ->
                let
                    updatedModel =
                        { model | user = Nothing }
                in
                    setRoute (Just Route.Home) updatedModel

            ProfilePageMsg subMsg ->
                case model.user of
                    Nothing ->
                        ( model, Cmd.none )

                    Just user ->
                        let
                            ( newUser, cmd ) =
                                ProfilePage.update subMsg user
                        in
                            ( { model | user = Just newUser }, cmd )

            HomePageMsg subMsg ->
                update (SetRoute (Just Route.NewForm)) model

            NewFormPageMsg subMsg ->
                ( model, NewFormPage.update subMsg model.user )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | activePage = NotFound }, Cmd.none )

        Just Route.Home ->
            ( { model | activePage = Home }, Cmd.none )

        Just Route.Login ->
            ( { model | activePage = Login }, Ports.startAuthUI () )

        Just Route.Logout ->
            ( { model | activePage = Home }, Ports.logOut () )

        Just Route.MyForms ->
            ( { model | activePage = MyForms }, Cmd.none )

        Just Route.Profile ->
            ( { model | activePage = Profile }, Cmd.none )

        Just Route.NewForm ->
            ( { model | activePage = NewForm }, Cmd.none )



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


navStyle : Page -> Page -> Styles
navStyle activePage navPage =
    if (activePage == navPage) then
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
