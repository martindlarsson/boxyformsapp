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
import Data.Form as Form exposing (..)


type alias Model =
    { activePage : Page
    , session : Session
    }


type Page
    = Blank
    | NotFound
    | Home
    | NewForm
    | MyForms
    | Login



-- | Errored PageLoadError
-- | Home HomePage.Model
-- | NewForm NewForm.Model
-- | MyForms MyForms.Model
-- | Login LoginPage.Model


initialPage : Page
initialPage =
    Blank


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    ( { activePage = initialPage
      , session = { user = Nothing }
      }
    , Cmd.none
      -- TODO ladda befintliga formulär
    )


view : Model -> Html Msg
view model =
    let
        page =
            model.activePage
    in
        Element.layout stylesheet <|
            grid Main
                [ padding 20, spacing 10 ]
                { columns = [ percent 100 ]
                , rows = [ px 80, fill ]
                , cells =
                    [ cell
                        { start = ( 0, 0 )
                        , width = 1
                        , height = 1
                        , content = navigation page
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


navigation : Page -> Element Styles variation msg
navigation activePage =
    row None
        [ spread, paddingXY 80 20 ]
        [ link (routeToString Route.Home) <| el Logo [] (Element.text "Boxyforms")
        , row None
            [ spacing 20 ]
            [ link (routeToString Route.Login) <| el (navStyle activePage Login) [] (Element.text "Logga in")
            , link (routeToString Route.Login) <| el (navStyle activePage NewForm) [] (Element.text "Nytt formulär")
            ]
        ]


navStyle : Page -> Page -> Styles
navStyle activePage navPage =
    if (activePage == navPage) then
        ActiveNavOption
    else
        NavOption


viewPage : Page -> Element style variation Msg
viewPage page =
    case page of
        Blank ->
            Element.text "Tomt"

        NotFound ->
            Element.text "Sidan inte funnen"

        Home ->
            Element.text "Hemma"

        MyForms ->
            Element.text "Mina formulär"

        NewForm ->
            Element.text "Nytt formulär"

        Login ->
            Element.text "Logga in"



-- UPDATE --


type Msg
    = NoOp
    | SetRoute (Maybe Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetRoute route ->
            setRoute route model


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | activePage = NotFound }, Cmd.none )

        Just Route.Home ->
            ( { model | activePage = Home }, Cmd.none )

        Just Route.Login ->
            ( { model | activePage = Login }, Cmd.none )

        Just Route.Logout ->
            ( { model | activePage = Home }, Cmd.none )

        Just Route.MyForms ->
            ( { model | activePage = MyForms }, Cmd.none )

        Just (Route.Profile username) ->
            ( { model | activePage = Home }, Cmd.none )

        Just Route.NewForm ->
            ( { model | activePage = NewForm }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
