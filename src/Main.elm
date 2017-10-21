module Main exposing (..)

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
import Page.Home as HomePage exposing (view)
import Page.Profile as ProfilePage exposing (view, update, Msg)


type alias Model =
    { activePage : Page
    , user : Maybe User

    -- , session : Session
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

      --   , session = { user = Nothing }
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        page =
            model.activePage
    in
        Element.viewport stylesheet <|
            grid Main
                [ padding 20, spacing 10 ]
                { columns = [ percent 100 ]
                , rows = [ px 80, fill ]
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
                                [ spacing 20, padding 20, paddingTop 50, paddingBottom 50 ]
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
        row None
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


navStyle : Page -> Page -> Styles
navStyle activePage navPage =
    if (activePage == navPage) then
        ActiveNavOption
    else
        NavOption


viewPage : Page -> Model -> Element Styles variation Msg
viewPage page model =
    case page of
        NotFound ->
            Element.text "Sidan inte funnen"

        Home ->
            HomePage.view

        MyForms ->
            Element.text "Mina formulär"

        NewForm ->
            Element.text "Nytt formulär"

        Profile ->
            let
                user =
                    model.user
            in
                case user of
                    Nothing ->
                        Element.text "ERROR!! det finns ingen inloggad användare..."

                    Just user ->
                        Element.map ProfilePageMsg (ProfilePage.view user.userData)

        Login ->
            LoginPage.view



-- UPDATE --


type Msg
    = NoOp
    | SetRoute (Maybe Route)
    | ReceivedUser Value
    | UserLoggedOut
    | ReceivedUserData Value
    | ProfilePageMsg ProfilePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update" msg
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            SetRoute route ->
                let
                    isUserDataOK =
                        validateUser model.user

                    _ =
                        Debug.log "SetRoute validateUser" isUserDataOK
                in
                    case isUserDataOK of
                        UserIsOK ->
                            setRoute route model

                        UserNeedsMoreInfo ->
                            setRoute (Just Route.Profile) model

                        NotLoggedIn ->
                            setRoute (Just Route.Login) model

            -- En användare har loggat in eller skapats
            ReceivedUser value ->
                value
                    |> Decode.decodeValue User.decoder
                    |> Result.map (\user -> { model | user = Just user } ! [ Ports.getUserData user.id ])
                    |> Result.withDefault (model ! [])

            -- Vi har fått ytterligare information om användarens konto
            ReceivedUserData value ->
                value
                    |> Decode.decodeValue User.dataDecoder
                    |> Result.map (\userData -> updateUserData model (Just userData) ! [])
                    |> Result.withDefault (setRoute (Just Route.Profile) model)

            UserLoggedOut ->
                let
                    updatedModel =
                        { model | user = Nothing }
                in
                    setRoute (Just Route.Home) updatedModel

            ProfilePageMsg subMsg ->
                ( updateUser model (ProfilePage.update subMsg model.user), Cmd.none )


updateUser : Model -> Maybe User -> Model
updateUser model newUser =
    { model | user = newUser }


updateUserData : Model -> Maybe UserData -> Model
updateUserData model newUserData =
    case model.user of
        Nothing ->
            model

        Just user ->
            let
                newUser =
                    { user | userData = newUserData }
            in
                updateUser model (Just newUser)


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
        [ Ports.receiveUser ReceivedUser
        , Ports.userLoggedOut (\_ -> UserLoggedOut)
        , Ports.receiveUserData ReceivedUserData
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
