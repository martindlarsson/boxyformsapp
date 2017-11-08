module Views.Form exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)


infoBox : String -> Element Styles variation msg
infoBox infoString =
    row InfoBoxBody [ padding 10 ] [ Element.text infoString ]
        |> above
            [ row InfoBoxTop [ height (px 3) ] [ empty ] ]


textInput : String -> String -> String -> (String -> msg) -> Element Styles variation msg
textInput fieldLabel placeholder textValue msg =
    Input.text Field
        [ padding 10 ]
        { onChange = msg
        , value = textValue
        , label =
            Input.placeholder
                { label = Input.labelAbove (el None [ verticalCenter, padding 10 ] (Element.text fieldLabel))
                , text = placeholder
                }
        , options =
            [ Input.errorBelow (el Error [ spacing 5 ] (Element.text "Detta fält måste fyllas i"))
            ]
        }


button : String -> msg -> Element Styles variation msg
button buttonText msg =
    row Box
        [ center, verticalCenter, height (px 40), onClick msg ]
        [ (Element.text buttonText) ]
