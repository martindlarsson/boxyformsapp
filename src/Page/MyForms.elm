module Page.MyForms exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session)
import Util exposing ((=>))
import Data.Form as Form exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


-- import Data.User as User exposing (User)
-- import Html.Events exposing (..)
-- import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
-- import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
-- import Request.User exposing (storeSession)
-- import Route exposing (Route)
-- MODEL --


type alias Model =
    { myForms : List Form
    , currentForm : Maybe Form
    }


initialModel : Model
initialModel =
    { myForms = []
    , currentForm = Nothing
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "container mt-4" ]
        [ viewCreateNew
        , table [ class "table" ]
            [ thead [ class "thead-default" ]
                [ th [] [ text "Namn" ]
                , th [] [ text "Publikt" ]
                , th [] [ text "Öppnar" ]
                , th [] [ text "Stänger" ]
                , th [] [ text "" ]
                ]
            , tbody []
                [ tr []
                    [ td [ attribute "scope" "row" ] [ text "Utmaningen 2017" ]
                    , td [] [ text "Ja" ]
                    , td [] [ text "1/1 2017" ]
                    , td [] [ text "23/6 2017" ]
                    , td [] [ button [ class "btn btn-secondary" ] [ span [] [ i [ class "fa fa-pencil fa-fw" ] [] ] ] ]
                    ]
                ]
            ]
        ]


viewCreateNew : Html Msg
viewCreateNew =
    div [ class "form-group" ]
        [ button [ class "btn btn-secondary" ] [ text "Nytt formulär" ]
        ]



-- UPDATE --


type Msg
    = CreateNew
    | ViewForm FormId


type ExternalMsg
    = NoOp


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        CreateNew ->
            { model | currentForm = Just emptyForm }
                => Cmd.none
                => NoOp

        ViewForm formId ->
            { model | currentForm = Just emptyForm }
                => Cmd.none
                => NoOp
