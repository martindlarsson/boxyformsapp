module Event.EventListView exposing(..)

import Html exposing (Html, div, text, program, h1)
import Models exposing(Model)
import Event.Models exposing(Event)
import Messages exposing(Msg(..))

import Material.Color as Color
import Material.Card as Card
import Material.Options as Options exposing (css, onMouseEnter, onMouseLeave)
import Material.Elevation as Elevation

white : Options.Property c m
white =
  Color.text Color.white

eventListView : Model -> Html Msg
eventListView model =
    div []
    (List.map (\event -> eventView event) model.events)
    --     eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    -- , eventView model
    

eventView : Event -> Html Msg
eventView event =
    Card.view
        [ css "width" "350px"
        , css "margin" "30px"
        , css "float" "left"
        , Color.background (Color.color Color.LightGreen Color.S200)
        -- Click
        , Options.onClick EventClicked
        , Elevation.e2
        ]
        [ Card.title [  ] [ Card.head [ ] [ text event.eventName ] ]
        , Card.text [  ] [ text event.organizer ]
        , Card.actions [ Card.border ] [ text event.webLink ]
        ]

