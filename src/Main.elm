module Main exposing (..)

import Navigation exposing (Location)
import Html exposing (..)
import Route exposing (Route)
import Util exposing ((=>))
import Views.PageView as PageView exposing (ActivePage)
import Page.HomePage as HomePage
import Page.NotFound as NotFound
import Page.ErrorPage as ErrorPage exposing (PageLoadError)
import Page.Login as LoginPage exposing (initialModel)
import Page.NewForm as NewForm
import Page.MyForms as MyForms
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, decoder)
import Data.Form as Form exposing (..)
import Json.Decode as Decode exposing (Value, decodeValue)
import Ports as Ports exposing (..)


-- import Page.Register as Register


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home HomePage.Model
    | NewForm NewForm.Model
    | MyForms MyForms.Model
    | Login LoginPage.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { pageState : PageState
    , session : Session
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = { user = decodeUserFromJson val }
        }


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            PageView.frame isLoading session.user
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> frame PageView.Other

            Blank ->
                -- This is for the very intiial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame PageView.Other

            Errored subModel ->
                ErrorPage.view session subModel
                    |> frame PageView.Other

            Home subModel ->
                HomePage.view session subModel
                    |> frame PageView.Home
                    |> Html.map HomeMsg

            NewForm subModel ->
                NewForm.view session subModel
                    |> frame PageView.Other
                    |> Html.map NewFormMsg

            MyForms subModel ->
                MyForms.view session subModel
                    |> frame PageView.Other
                    |> Html.map MyFormsMsg

            Login subModel ->
                LoginPage.view session subModel
                    |> frame PageView.Other


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result String (List Form))
    | HomeMsg HomePage.Msg
    | MyFormsMsg MyForms.Msg
    | ReceiveUser Value
    | NewFormMsg NewForm.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        fromPage =
            getPage model.pageState

        transition cmd =
            case fromPage of
                Login _ ->
                    { model | pageState = TransitioningFrom fromPage }
                        => Cmd.batch [ Ports.deleteFBUI (), cmd ]

                _ ->
                    { model | pageState = TransitioningFrom fromPage }
                        => cmd

        errored =
            pageErrored model
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just Route.Home ->
                transition (HomePage.init model.session)

            Just Route.NewForm ->
                { model | pageState = Loaded (NewForm NewForm.initialModel) } => Cmd.none

            Just Route.MyForms ->
                { model | pageState = Loaded (MyForms MyForms.initialModel) } => Ports.deleteFBUI ()

            Just Route.Login ->
                { model | pageState = Loaded (Login LoginPage.initialModel) } => Ports.startAuthUI ()

            Just Route.Logout ->
                { model | session = { user = Nothing } } => Ports.logOut ()

            Just _ ->
                transition (HomePage.init model.session)


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            ErrorPage.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none


toPageLoadError : ActivePage -> String -> PageLoadError
toPageLoadError activePage errorMessage =
    ErrorPage.pageLoadError activePage errorMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( HomeLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (Home { forms = subModel }) } => Cmd.none

            ( HomeLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored (toPageLoadError PageView.Home error)) } => Cmd.none

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (HomePage.update session) subMsg subModel

            ( NewFormMsg subMsg, NewForm subModel ) ->
                toPage NewForm NewFormMsg (NewForm.update session) subMsg subModel

            -- En användare har loggat in eller skapats
            ( ReceiveUser value, _ ) ->
                value
                    |> Decode.decodeValue User.decoder
                    |> Result.map (\user -> { model | session = { user = Just user } } ! [])
                    |> Result.withDefault (model ! [])

            ( _, NotFound ) ->
                let
                    _ =
                        Debug.log "Update" "_,NotFound"
                in
                    -- Disregard incoming messages when we're on the
                    -- NotFound page.
                    model => Cmd.none

            ( _, _ ) ->
                let
                    _ =
                        Debug.log "SetRoute" "_,_"
                in
                    -- Disregard incoming messages that arrived for the wrong page
                    model => Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        getAllFormsSub =
            Ports.gotAllPublicForms (Decode.decodeValue jsonFormListDecoder)
    in
        Sub.batch
            [ Sub.map HomeLoaded getAllFormsSub
            , Ports.receiveUser ReceiveUser
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
