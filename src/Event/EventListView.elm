module Event.EventListView exposing(..)

import Html exposing (Html, div, text, program, h1)
import Models exposing(Model)
import Event.Models exposing(Event, EventForm)
import Messages exposing (Msg(..))

import Material
import Material.Color as Color
import Material.Card as Card
import Material.Options as Options exposing (css, onMouseEnter, onMouseLeave, onClick)
import Material.Elevation as Elevation
import Material.Button as Button --exposing (..)
import Routing exposing(formPath)


eventListView : Model -> Html Msg
eventListView model =
    div []
    (List.map (\event -> eventView event model.mdl) model.events)
    

eventView : Event -> Material.Model -> Html Msg
eventView event mdl =
    Card.view
        [ css "width" "350px"
        , css "margin" "30px"
        , css "float" "left"
        , Color.background (Color.color Color.LightGreen Color.S200)
        -- Click
        -- , Options.onClick EventClicked -- Maby fold out button list?
        , Elevation.e2
        ]
        [ Card.title [  ] [ Card.head [ ] [ text event.eventName ] ]
        , Card.text [  ] [ text event.organizer ]
        , Card.actions [ Card.border ] (List.map (\form -> formButton form mdl) event.eventForms)
        ]
    
formButton : EventForm -> Material.Model -> Html Msg
formButton eventForm mdl =
    Button.render Mdl [0] mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Button.link ( formPath eventForm.formId ) --Options.onClick (EventFormClicked eventForm.formId)
        ]
        [ text eventForm.formName]

