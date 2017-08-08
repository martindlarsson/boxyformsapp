module Page.MyForms exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

import Data.Session as Session exposing (Session)
import Util exposing ((=>))
import Data.Form as Form exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


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
    div [ class "container" ]
        [ table [ class "table" ]
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
