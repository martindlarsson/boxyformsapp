module Event.NewEventListView exposing (..)

import Html exposing (Html, div, text, table, tr, th, td)
import Html.Attributes exposing (style, class, href)
import Models exposing (Model, Route(..))
import Event.Models exposing (Event, EventForm)
import Messages exposing (Msg(..))


eventListView : Model -> Html Msg
eventListView model =
    let
        eventRows =
            (List.map (\event -> eventView event) model.events)
    in
        table
            [ class "table" ]
            (List.append
                [ tr
                    []
                    [ th [] [ text "Organisatör" ]
                    , th [] [ text "Namn" ]
                    ]
                ]
                eventRows
            )


eventView : Event -> Html Msg
eventView event =
    tr
        []
        [ td [] [ text event.organizer ]
        , td [] [ text event.eventName ]
        ]
