module Event.EventListView exposing(..)

import Html exposing (Html, div, text, program, h1)
import Models exposing(Model)
import Messages exposing(Msg(..))

import Material.Color as Color
import Material.Card as Card
import Material.Options as Options exposing (css, onMouseEnter, onMouseLeave)
import Material.Elevation as Elevation

eventListView : Model -> Html Msg
eventListView model =
    Card.view
        [ css "width" "350px"
        , Color.background (Color.color Color.Lime Color.S500)
        -- Click
        , Options.onClick EventClicked
        , Elevation.e2
        ]
        [ Card.title [ Color.white ] [ Card.head [ ] [ text "Click anywhere" ] ]
        , Card.text [ ] [ text "Lite text" ]
        , Card.actions [ Card.border ] [ text "(not here)" ]
        ]