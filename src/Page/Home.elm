module Page.Home exposing (view, update, init, Model, emptyModel, MsgForParent(..))

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Route exposing (routeToString)
import BoxyStyle exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form as FormData exposing (..)
import Data.User exposing (..)
import Color
import FeatherIcons


-- Model


type alias Model =
    { user : Maybe User
    , forms : List Form
    }


emptyModel : Model
emptyModel =
    Model Nothing []



-- Msg --


type MsgForParent
    = NewForm
    | EditForm Form
    | DeleteForm Form



-- Update --


update : MsgForParent -> ( Cmd msg, MsgForParent )
update msg =
    ( Cmd.none, msg )



-- let
--     _ =
--         Debug.log "update" (toString msg)
-- in
--     case msg of
--         NewFormMsg ->
--             ( Cmd.none, NoMsg )
--         ButtonPressed msgForParent ->
--             ( Cmd.none, msgForParent )
-- Init --


init : Maybe User -> List Form -> Model
init maybeUser forms =
    Model maybeUser forms



-- VIEW --


view : Model -> Element MsgForParent
view model =
    let
        isUserDataOK =
            validateUser model.user

        buttonText =
            case isUserDataOK of
                UserIsOK ->
                    "Skapa nytt formulär"

                UserNeedsMoreInfo ->
                    "Fyll i mer information i min profil"

                NotLoggedIn ->
                    "Logga in och skapa ditt första formulär"

        userView =
            if (List.isEmpty model.forms) then
                emptyView
            else
                formsView model.forms
    in
        column [ spacing 20, padding 10 ]
            [ paragraph (h1 ++ [ Element.centerX ]) [ text "Hej och välkommen till BoxyForms!" ]
            , verticalSpacing 20
            , userView
            , FormView.button buttonText NewForm [ Element.height (px 40) ] FormView.Enabled
            ]


emptyView : Element MsgForParent
emptyView =
    paragraph [] [ text "Här kan du skapa formulär för t.ex. anmälan till ditt sportarrangemang eller ert bröllop. Börja med att skapa ett formulär genom att klicka på knappen nedan." ]


formsView : List Form -> Element MsgForParent
formsView forms =
    column [ spacing 20 ] <| List.map (\form -> formView form) forms


formView : Form -> Element MsgForParent
formView form =
    row
        [ Background.color Color.white
        , Border.shadow
            { offset = ( 2, 2 )
            , blur = 3
            , size = 1
            , color = Color.gray
            }
        ]
        [ column [ width fill, spacing 10, padding 10 ]
            [ row [ Font.size 18 ] [ link [ alignLeft ] { url = routeToString <| Route.EditForm form.id, label = el [] (text form.name) } ]

            --[ el [] (text form.name) ]
            , row [ Font.size 10, Font.color Color.lightCharcoal ] [ el [] (text ("Öppet mellan " ++ form.dateFrom ++ " och " ++ form.dateTo)) ]
            ]
        , column [ width (px 100), Background.color Color.lightOrange ] [ buttonView form ]
        ]


buttonView : Form -> Element MsgForParent
buttonView form =
    row [ centerY, centerX, width (px 70) ]
        [ link [ alignLeft ] { url = routeToString <| Route.EditForm form.id, label = el [] (Element.html (FeatherIcons.edit |> FeatherIcons.toHtml [])) }
        , el [ alignRight, onClick (DeleteForm form), pointer ] (Element.html (FeatherIcons.trash |> FeatherIcons.toHtml []))
        ]
