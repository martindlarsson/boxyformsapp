module Page.HomePage exposing (Model, Msg(..), update, view, init)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, style)
import Data.Form as Form exposing (Form)
import Ports exposing (..)
import Data.Session as Session exposing (Session)


-- import Page.ErrorPage as ErrorPage exposing (PageLoadError, pageLoadError)
-- import Task exposing (Task)
-- import Views.PageView as Page
-- MODEL --


type alias Model =
    { forms : List Form
    }



-- TODO, ta in modellen på ngt sätt så att vi inte laddar om varje gång


init : Session -> Cmd msg
init session =
    Ports.getAllPublicForms ()



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ table [ class "table" ]
            [ thead [ class "thead-default" ]
                [ th [] [ text "Formulär" ]
                , th [] [ text "Arrangör" ]
                , th [] [ text "Öppen till" ]
                ]
            , tbody []
                (List.map (\form -> formRowView form) model.forms)
            ]
        ]


formRowView : Form -> Html Msg
formRowView form =
    tr []
        [ td [ attribute "scope" "row" ] [ a [ href ("/#/form/" ++ form.id) ] [ text form.name ] ]
        , td [] [ text form.orgName ]
        , td [] [ text form.closeDate ]
        ]



-- UPDATE --


type Msg
    = SelectForm
    | GotAllPublicFormsMsg (Result String (Maybe List Form))
    | GotFormMsg (Result String Form)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SelectForm ->
            ( model, Cmd.none )

        GotAllPublicFormsMsg forms ->
            ( model, Cmd.none )

        GotFormMsg result ->
            case result of
                Ok form ->
                    ( model, Cmd.none )

                Err errorMsg ->
                    let
                        _ =
                            Debug.log "GotFormMsg Error" errorMsg
                    in
                        ( model, Cmd.none )
