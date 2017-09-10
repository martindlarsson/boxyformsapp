module Views.Form exposing (input, password, textarea, viewErrors, dateTimePicker)

import Html exposing (Attribute, Html, fieldset, li, text, ul, span, div, i)
import Html.Attributes exposing (class, type_, id)


password : List (Attribute msg) -> List (Html msg) -> Html msg
password attrs =
    control Html.input ([ type_ "password" ] ++ attrs)


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attrs =
    control Html.input ([ type_ "text" ] ++ attrs)


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    control Html.textarea


dateTimePicker : List (Attribute msg) -> Html msg
dateTimePicker attributes =
    fieldset [ class "form-group" ]
        [ div [ class "input-group date", id "datepicker" ]
            [ Html.input ([ class "form-control", type_ "text" ] ++ attributes) []
            , span [ class "input-group-addon" ] [ i [ class "fa fa-calendar" ] [] ]
            ]
        ]


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "error-messages" ]



-- INTERNAL --


control :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
control element attributes children =
    fieldset [ class "form-group" ]
        [ element (class "form-control" :: attributes) children ]
