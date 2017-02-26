module View exposing(view)

import Html exposing (Html, div, text, h1)
import Html.Attributes exposing(style)
import Models exposing(Model)
import Messages exposing(Msg(..))
import Event.EventListView as EventListView exposing(..)

import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Scheme
-- import Material.Button as Button
-- import Material.Options as Options exposing (css)


-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Lime Color.LightBlue <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            ]
            { header = header model
            , drawer = []
            , tabs = ( [], [] )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    [ body model
                    , EventListView.eventListView model
                    -- , Snackbar.view model.snackbar |> App.map Snackbar
                    ]
                ]
            }

body : Model -> Html Msg
body model =
    div [] [ text "BODYYY" ]




header : Model -> List (Html Msg)
header model =
    [ h1 [ style [ ("text-align","center") ] ] [ text "BoxyForms" ] ]


-- header : Model -> Html Msg
-- header model =
--     [ Layout.Contents
--         []
--         [ Layout.title [] [ text "BoxyForms" ]
--         ]
--     ]