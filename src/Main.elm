module Main exposing (..)

import Data.Session as Session exposing (Session)
import Route exposing (Route, routeToString)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import BoxyStyle exposing (..)
import Ports exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (..)
import Data.User as User exposing (User, decoder)
import Page.Login as LoginPage exposing (view)
import Page.Home as HomePage exposing (view)


type alias Model =
    { activePage : Page
    , session : Session
    }


type Page
    = NotFound
    | Home
    | NewForm
    | MyForms
    | Login


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    ( { activePage = Home
      , session = { user = Nothing }
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
                                [ spacing 50, padding 20, paddingTop 50, paddingBottom 50 ]
                                (viewPage page)
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
            model.session.user

        activePage =
            model.activePage
    in
        if (user == Nothing) then
            [ link (routeToString Route.Login) <| el (navStyle activePage Login) [] (Element.text "Logga in") ]
        else
            [ link (routeToString Route.NewForm) <| el (navStyle activePage NewForm) [] (Element.text "Nytt formulär")
            , link (routeToString Route.MyForms) <| el (navStyle activePage MyForms) [] (Element.text "Mina formulär")
            , link (routeToString Route.Logout) <| el NavOption [] (Element.text "Logga ut")
            ]


navStyle : Page -> Page -> Styles
navStyle activePage navPage =
    if (activePage == navPage) then
        ActiveNavOption
    else
        NavOption


viewPage : Page -> Element Styles variation Msg
viewPage page =
    case page of
        NotFound ->
            Element.text "Sidan inte funnen"

        Home ->
            HomePage.view

        MyForms ->
            Element.text "Mina formulär"

        NewForm ->
            Element.text "Nytt formulär"

        Login ->
            LoginPage.view



-- UPDATE --


type Msg
    = NoOp
    | SetRoute (Maybe Route)
    | ReceivedUser Value
    | UserLoggedOut


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
                setRoute route model

            -- En användare har loggat in eller skapats
            ReceivedUser value ->
                value
                    |> Decode.decodeValue User.decoder
                    |> Result.map (\user -> { model | session = { user = Just user } } ! [])
                    |> Result.withDefault (model ! [])

            UserLoggedOut ->
                ( { model | session = { user = Nothing } }, Cmd.none )


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

        Just (Route.Profile username) ->
            ( { model | activePage = Home }, Cmd.none )

        Just Route.NewForm ->
            ( { model | activePage = NewForm }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.receiveUser ReceivedUser, Ports.userLoggedOut (\_ -> UserLoggedOut) ]



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
