module View exposing(view)

import Html exposing (Html, div, text, program, h1)
import Html.Attributes exposing(style)
import Models exposing(Model)
import Messages exposing(Msg(..))

import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Scheme
-- import Material.Button as Button
-- import Material.Options as Options exposing (css)


-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.BlueGrey Color.LightBlue <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedDrawer
            ]
            { header = header model
            -- , drawer = drawer model
            -- , tabs = ( [], [] )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    [ body model
                    -- , Snackbar.view model.snackbar |> App.map Snackbar
                    ]
                ]
            }

body : Model -> Html Msg
body model =
    div [] [ text "BODYYY" ]

header : Model -> List (Html Msg)
header model =
    [ Layout.Contents
        []
        [ Layout.title [] [ text "BoxyForms" ]
        ]
    ]