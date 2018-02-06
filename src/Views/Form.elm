module Views.Form exposing (..)

import Element as El exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Background as Background
import Element.Border as Border
import Color
import BoxyStyle exposing (..)


type Ability
    = Disabled
    | Enabled


type TextInputType
    = Multiline
    | Singleline


infoBox : String -> Element msg
infoBox infoString =
    row [ Background.color Color.lightBlue, height (px 3) ]
        [ paragraph [ Background.color Color.white, padding 10 ] [ El.text infoString ] ]


textInput : TextInputType -> String -> String -> String -> (String -> msg) -> Ability -> Element msg
textInput inputType fieldLabel placeholder textValue msg ability =
    let
        onChangeMsg =
            if (ability == Disabled) then
                Nothing
            else
                Just msg

        fieldStyle =
            if (ability == Disabled) then
                [ Border.width 1, Border.solid, Border.color Color.lightCharcoal, Background.color Color.grey ]
            else
                [ Border.width 1, Border.solid, Border.color Color.lightCharcoal ]

        input =
            if (inputType == Multiline) then
                Input.multiline
            else
                Input.text
    in
        input fieldStyle
            { onChange = onChangeMsg
            , text = textValue
            , placeholder = Just <| Input.placeholder [] (El.text placeholder)
            , label = Input.labelAbove [] (El.text fieldLabel)
            , notice = Nothing
            }



-- textInput : TextInputType -> String -> String -> String -> (String -> msg) -> Ability -> Element msg
-- textInput inputType fieldLabel placeholder textValue msg ability =
--     let
--         abilityOption =
--             if (ability == Disabled) then
--                 [ disabled ]
--             else
--                 []
--         fieldStyle =
--             if (ability == Disabled) then
--                 [ Border.width 1, Border.solid, Border.color Color.lightCharcoal, Background.color Color.grey ]
--             else
--                 [ Border.width 1, Border.solid, Border.color Color.lightCharcoal ]
--         input =
--             if (inputType == Multiline) then
--                 Input.multiline
--             else
--                 Input.text
--     in
--         input fieldStyle
--             [ padding 10 ]
--             { onChange = msg
--             , value = textValue
--             , label =
--                 Input.placeholder
--                     { label = Input.labelAbove (el [] [ centerY ] (El.text fieldLabel))
--                     , text = placeholder
--                     }
--             , options =
--                 List.append
--                     abilityOption
--                     [--Input.errorBelow (el Error [ spacing 5 ] (Element.text "Detta fält måste fyllas i"))
--                     ]
--             }


button : String -> msg -> List (Attribute msg) -> Element msg
button buttonText msg layout =
    row (List.concat [ box, layout, [ onClick msg ] ])
        [ (El.text buttonText) ]


checkbox : String -> (Bool -> msg) -> Bool -> Element msg
checkbox labelText msg value =
    Input.checkbox []
        { onChange = Just msg
        , icon = Nothing
        , checked = value
        , label = Input.labelRight [] (El.text labelText)
        , notice = Nothing
        }
