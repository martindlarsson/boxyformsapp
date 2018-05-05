module Main exposing (..)

import Window as Window exposing (..)
import Task exposing (..)
import Route exposing (Route, routeToString)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import Ports exposing (..)
import Element as El exposing (..)
import Element.Border as Border exposing (..)
import Element.Keyed as Keyed exposing (..)
import Element.Background as Background
import Color
import Element.Font as Font
import Html exposing (..)
import Data.User as User exposing (..)
import Data.Form as Form exposing (..)
import Page.Login as LoginPage exposing (view)
import Page.Home as HomePage exposing (view, update, MsgForParent(..))
import Page.Profile as ProfilePage exposing (view, update, Msg(..))
import Page.NewForm as NewFormPage exposing (view, update, Msg, MsgForParent(..))


type alias Model =
    { activePage : Page
    , activeRoute : Route
    , user : Maybe User
    , userForms : List Form
    , statusMessage : Maybe MessageType
    , device : Maybe Device -- size of the window classified
    }


type MessageType
    = InfoMessage String
    | ErrorMessage String


type Page
    = NotFound
    | Home HomePage.Model
    | NewForm NewFormPage.Model
    | Login
    | Profile ProfilePage.Model


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    ( { activePage = Home HomePage.emptyModel
      , activeRoute = Route.Home
      , user = Nothing
      , userForms = []
      , statusMessage = Nothing
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
                [ El.width fill, paddingXY 0 20 ]
            else if (device.tablet) then
                [ El.width (px 600), centerX, paddingXY 0 20 ]
            else
                [ El.width (px 800), centerX, paddingXY 0 20 ]

        message =
            case model.statusMessage of
                Nothing ->
                    El.none

                Just messageType ->
                    showMessage messageType
    in
        El.layout
            [ Font.family [ Font.sansSerif, Font.typeface "Open Sans" ]
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
                , message
                , El.row
                    --El.height <| px (device.height - 80)
                    pageLayout
                    [ (viewPage page model) ]
                ]


showMessage : MessageType -> Element msg
showMessage messageType =
    let
        ( fontColor, borderColor ) =
            case messageType of
                InfoMessage m ->
                    ( Color.black, Color.white )

                ErrorMessage m ->
                    ( Color.red, Color.red )

        message =
            case messageType of
                InfoMessage m ->
                    m

                ErrorMessage m ->
                    m
    in
        El.row
            [ El.height (px 60)
            , Background.color Color.darkGray
            , Border.dashed
            , Border.color Color.white
            , Border.width 4
            , Font.color fontColor
            ]
            [ El.paragraph
                [ El.centerX
                , El.centerY
                ]
                [ El.text message ]
            ]


navigation : Model -> Element msg
navigation model =
    let
        activePage =
            model.activePage
    in
        El.row
            [ centerY
            , centerX
            , padding 20
            ]
            [ link []
                { url = (routeToString Route.Home)
                , label = (El.el [ Font.size 25 ] (El.text "BoxyForms"))
                }
            , El.row [ El.width fill ] [ El.none ]
            , El.row
                [ spacing 20 ]
                (signInLink model)
            ]


signInLink : Model -> List (Element msg)
signInLink model =
    let
        user =
            model.user

        activeRoute =
            model.activeRoute

        userState =
            validateUser user

        alignR =
            [ alignRight ]
    in
        if (user == Nothing) then
            [ link alignR { url = (routeToString Route.Login), label = (El.el (navStyle activeRoute Route.Login) (El.text "Logga in")) } ]
        else if (userState == UserNeedsMoreInfo) then
            [ link alignR { url = (routeToString Route.Logout), label = (El.el [ Font.size 16 ] (El.text "Logga ut")) } ]
        else
            [ --link [] { url = (routeToString Route.NewForm), label = (El.el (navStyle activeRoute Route.NewForm) (El.text "Nytt formulär")) }
              link alignR { url = (routeToString Route.Profile), label = (El.el (navStyle activeRoute Route.Profile) (El.text "Min profil")) }
            , link alignR { url = (routeToString Route.Logout), label = (El.el [ Font.size 16 ] (El.text "Logga ut")) }
            ]


viewPage : Page -> Model -> Element Msg
viewPage page model =
    case page of
        NotFound ->
            El.text "Sidan inte funnen"

        Home pageModel ->
            El.map HomePageMsg (HomePage.view pageModel)

        NewForm pageModel ->
            Keyed.row [] [ ( "new_form", (El.map NewFormPageMsg (NewFormPage.view pageModel)) ) ]

        Profile model ->
            Keyed.row [] [ ( "profile_page", (El.map ProfilePageMsg (ProfilePage.view model)) ) ]

        Login ->
            LoginPage.view



-- UPDATE --


type Msg
    = NoOp
    | WindowResize Window.Size
    | SetRoute (Maybe Route)
    | UserLoggedIn Value
    | UserLoggedOut
    | UserSaved Value
    | ProfilePageMsg ProfilePage.Msg
    | HomePageMsg HomePage.MsgForParent
    | NewFormPageMsg NewFormPage.Msg
    | FormSaved String
    | GotForms Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- _ =
        --     Debug.log "update" msg
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
                        { model | user = Nothing, userForms = [] }
                in
                    setRoute (Just Route.Home) updatedModel

            ( UserSaved value, _ ) ->
                let
                    userResult =
                        Decode.decodeValue User.decoder value

                    loggedInUser =
                        case userResult of
                            Ok user ->
                                Just user

                            Err str ->
                                Nothing
                in
                    case ( model.activePage, loggedInUser ) of
                        ( Profile oldPageModel, Just newUser ) ->
                            let
                                ( newPageModel, cmd ) =
                                    ProfilePage.update (ProfilePage.UserSaved newUser) oldPageModel

                                infoMessage =
                                    InfoMessage "Uppgifterna är sparade! :)"
                            in
                                ( { model | activePage = Profile newPageModel, user = loggedInUser, statusMessage = Just infoMessage }, cmd )

                        _ ->
                            ( { model | user = loggedInUser }, Cmd.none )

            ( ProfilePageMsg subMsg, Profile subModel ) ->
                let
                    ( pageModel, cmd ) =
                        ProfilePage.update subMsg subModel
                in
                    ( { model | activePage = Profile pageModel }, cmd )

            ( HomePageMsg subMsg, _ ) ->
                let
                    ( cmd, msgFromPage ) =
                        HomePage.update subMsg
                in
                    case msgFromPage of
                        HomePage.EditForm form ->
                            update (SetRoute (Just <| Route.EditForm form.id)) model

                        HomePage.DeleteForm form ->
                            update (SetRoute (Just <| Route.EditForm form.id)) model

                        HomePage.NewForm ->
                            update (SetRoute (Just Route.NewForm)) model

            ( NewFormPageMsg subMsg, NewForm subModel ) ->
                let
                    ( ( newFormPageModel, cmd ), msgFromPage ) =
                        NewFormPage.update subMsg subModel

                    updatedForms =
                        case msgFromPage of
                            NewFormPage.AddNewForm form ->
                                -- Add new form or update the old
                                updateListItem model.userForms form.id form

                            NewFormPage.NoMsg ->
                                model.userForms
                in
                    ( { model | activePage = NewForm newFormPageModel, userForms = updatedForms }, Cmd.map NewFormPageMsg cmd )

            -- Ett formulär har sparats
            ( FormSaved formId, _ ) ->
                case model.activePage of
                    NewForm pageModel ->
                        let
                            ( ( newFormPageModel, cmd ), msgFromPage ) =
                                NewFormPage.update NewFormPage.FormSaved pageModel

                            infoMessage =
                                InfoMessage "Formuläret sparades"
                        in
                            ( { model | activePage = NewForm newFormPageModel, statusMessage = Just infoMessage }, Cmd.map NewFormPageMsg cmd )

                    _ ->
                        ( model, Cmd.none )

            ( GotForms formsObject, activePage ) ->
                let
                    formsResult =
                        Form.decodeForms formsObject

                    ( forms, errorMessage ) =
                        case formsResult of
                            Ok forms ->
                                ( forms, Nothing )

                            Err str ->
                                ( [], Just <| ErrorMessage str )

                    newActivePage =
                        case activePage of
                            Home pageModel ->
                                Home { pageModel | forms = forms }

                            page ->
                                page
                in
                    ( { model | userForms = forms, statusMessage = errorMessage, activePage = newActivePage }, Cmd.none )

            ( _, _ ) ->
                -- Throw away any stray messages
                ( model, Cmd.none )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        device =
            case model.device of
                Nothing ->
                    initialDevice

                Just currentDevice ->
                    currentDevice
    in
        case model.user of
            Nothing ->
                -- TODO set error message!!
                ( { model | activePage = Home HomePage.emptyModel, activeRoute = Route.Home }, Cmd.none )

            Just logedInUser ->
                case maybeRoute of
                    Nothing ->
                        ( { model | activePage = NotFound, activeRoute = Route.Home }, Cmd.none )

                    Just Route.Home ->
                        let
                            cmd =
                                case model.user of
                                    Nothing ->
                                        Cmd.none

                                    Just user ->
                                        getMyForms user.id

                            pageModel =
                                HomePage.init model.user model.userForms
                        in
                            ( { model | activePage = Home pageModel, activeRoute = Route.Home }, cmd )

                    Just Route.Login ->
                        ( { model | activePage = Login, activeRoute = Route.Login }, Ports.startAuthUI () )

                    Just Route.Logout ->
                        ( { model | activePage = Home HomePage.emptyModel, activeRoute = Route.Home }, Ports.logOut () )

                    Just Route.Profile ->
                        let
                            persistentUserState =
                                validateUser <| Just logedInUser

                            profilePageModel =
                                ProfilePage.init logedInUser persistentUserState
                        in
                            ( { model | activePage = (Profile profilePageModel), activeRoute = Route.Profile }, Cmd.none )

                    Just Route.NewForm ->
                        let
                            ( newFormModel, newFormCmd ) =
                                NewFormPage.init logedInUser device Nothing
                        in
                            ( { model | activePage = (NewForm newFormModel), activeRoute = Route.NewForm }, Cmd.map NewFormPageMsg newFormCmd )

                    Just (Route.EditForm formId) ->
                        let
                            form =
                                getListItem model.userForms formId

                            ( newFormModel, newFormCmd ) =
                                NewFormPage.init logedInUser device form
                        in
                            -- TODO om form är Nothing, visa felmeddelande!!
                            ( { model | activePage = (NewForm newFormModel), activeRoute = (Route.EditForm formId) }, Cmd.map NewFormPageMsg newFormCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.userLoggedIn UserLoggedIn
        , Ports.userLoggedOut (\_ -> UserLoggedOut)
        , Ports.userSaved UserSaved
        , Window.resizes (\wSize -> WindowResize wSize)
        , Ports.formSaved FormSaved
        , Ports.gotForms GotForms
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
        , alignRight
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


initialDevice : Device
initialDevice =
    classifyDevice
        { width = 400
        , height = 400
        }


pageToString : Page -> String
pageToString page =
    case page of
        NotFound ->
            "Notfound"

        Home model ->
            "Hem"

        NewForm model ->
            "Nytt formulär"

        Login ->
            "Logga in"

        Profile mode ->
            "Min profil"
