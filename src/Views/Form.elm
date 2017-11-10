module Views.Form exposing (..)

import Element as El exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)


type Ability
    = Disabled
    | Enabled


infoBox : String -> Element Styles variation msg
infoBox infoString =
    above
        [ row InfoBoxTop [ height (px 3) ] [ empty ] ]
        (paragraph InfoBoxBody [ padding 10 ] [ El.text infoString ])


textInput : String -> String -> String -> (String -> msg) -> Ability -> Element Styles variation msg
textInput fieldLabel placeholder textValue msg ability =
    let
        abilityOption =
            if (ability == Disabled) then
                [ disabled ]
            else
                []

        fieldStyle =
            if (ability == Disabled) then
                DisabledField
            else
                Field
    in
        Input.text fieldStyle
            [ padding 10 ]
            { onChange = msg
            , value = textValue
            , label =
                Input.placeholder
                    { label = Input.labelAbove (el None [ verticalCenter ] (El.text fieldLabel))
                    , text = placeholder
                    }
            , options =
                List.append
                    abilityOption
                    [--Input.errorBelow (el Error [ spacing 5 ] (Element.text "Detta fält måste fyllas i"))
                    ]
            }


button : String -> msg -> Element Styles variation msg
button buttonText msg =
    row Box
        [ center, verticalCenter, height (px 40), onClick msg ]
        [ (El.text buttonText) ]
