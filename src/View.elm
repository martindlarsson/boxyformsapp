module View exposing (view)

import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (style)
import Models exposing (Model, Route(..))
import Messages exposing (Msg(..))
import Event.EventListView as EventListView exposing (..)
-- import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Scheme
import Material.Footer as Footer exposing (mini)
import Material.Options as Options exposing (css)


-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Lime Color.DeepOrange <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.scrolling
            , Layout.fixedDrawer
            ]
            { header = header model
            , drawer = []
            , tabs = ( [], [] )
            , main =
                [ div
                    [ style [ ( "padding", "1rem" ) ] ]
                    (body model) --[ EventListView.eventListView model ]
                , footer model
                ]
            }


header : Model -> List (Html Msg)
header model =
    [ Layout.row [] [ Layout.title [] [ text "BoxyForms" ] ] ]


body : Model -> List (Html Msg)
body model =
    case model.route of
        EventListRoute ->
            [ EventListView.eventListView model ]

        FormRoute formId ->
            [ text ( "formId: " ++ ( toString formId) ) ]

        NotFoundRoute ->
            [ notFoundView ]


-- Footer


footer : Model -> Html Msg
footer model =
    Footer.mini
        [ css "position" "relative"
        , css "width" "100%"
        , css "height" "80px"
        , css "overflow" "hidden"
        ]
        { left =
            Footer.left []
                [ Footer.logo [] [ Footer.html <| text "Mini Footer Example" ]
                , Footer.links []
                    [ Footer.linkItem [ Footer.href "#footers" ] [ Footer.html <| text "Link 1" ]
                    , Footer.linkItem [ Footer.href "#footers" ] [ Footer.html <| text "Link 2" ]
                    , Footer.linkItem [ Footer.href "#footers" ] [ Footer.html <| text "Link 3" ]
                    ]
                ]
        , right =
            Footer.right []
                [ Footer.logo [] [ Footer.html <| text "Mini Footer Right Section" ]
                , Footer.socialButton [ Options.css "margin-right" "6px" ] []
                , Footer.socialButton [ Options.css "margin-right" "6px" ] []
                , Footer.socialButton [ Options.css "margin-right" "0px" ] []
                ]
        }


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]
