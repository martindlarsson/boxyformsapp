module Views.Form exposing (..)

import Element as El exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)


type Ability
    = Disabled
    | Enabled


type TextInputType
    = Multiline
    | Singleline


infoBox : String -> Element Styles variation msg
infoBox infoString =
    above
        [ row InfoBoxTop [ height (px 3) ] [ empty ] ]
        (paragraph InfoBoxBody [ padding 10 ] [ El.text infoString ])


textInput : TextInputType -> String -> String -> String -> (String -> msg) -> Ability -> Element Styles variation msg
textInput inputType fieldLabel placeholder textValue msg ability =
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

        input =
            if (inputType == Multiline) then
                Input.multiline
            else
                Input.text
    in
        input fieldStyle
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


button : String -> msg -> List (Attribute variation msg) -> Element Styles variation msg
button buttonText msg layout =
    row Box
        (List.append
            layout
            [ center, verticalCenter, height (px 40), onClick msg ]
        )
        [ (El.text buttonText) ]


checkbox : String -> (Bool -> msg) -> Bool -> Element Styles variation msg
checkbox labelText msg value =
    Input.checkbox None
        [ padding 10 ]
        { onChange = msg
        , checked = value
        , label = el None [] (El.text labelText)
        , options = []
        }
