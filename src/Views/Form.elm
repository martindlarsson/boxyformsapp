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
    El.paragraph
        [ Background.color Color.gray
        , padding 10
        , Border.dashed
        , Border.color Color.white
        , Border.width 4]
        [ El.text infoString ]


textInput : TextInputType -> Maybe String -> String -> String -> (String -> msg) -> Ability -> Element msg
textInput inputType fieldLabel placeholder textValue msg ability =
    let
        onChangeMsg =
            if (ability == Disabled) then
                Nothing
            else
                Just msg

        fieldStyle =
            if (ability == Disabled) then
                [ Border.width 1, Border.solid, Border.color Color.lightCharcoal, spacing 5, Background.color Color.grey, centerY ]
            else
                [ Border.width 1, Border.solid, Border.color Color.lightCharcoal, spacing 5, centerY ]

        boxHeight =
            if (inputType == Singleline) then
                [ height (px 40) ]
            else
                [ height (px 60) ]

        theLabel =
            case fieldLabel of
                Just fLabel ->
                    Input.labelAbove [] (El.text fLabel)

                Nothing ->
                    Input.labelAbove [] (El.empty)
    in
        case inputType of
            Multiline ->
                Input.multiline (List.append fieldStyle boxHeight)
                    { onChange = onChangeMsg
                    , text = textValue
                    , placeholder = Just <| Input.placeholder [] (El.text placeholder)
                    , label = theLabel
                    , spellcheck = False
                    }

            Singleline ->
                Input.text (List.append fieldStyle boxHeight)
                    { onChange = onChangeMsg
                    , text = textValue
                    , placeholder = Just <| Input.placeholder [] (El.text placeholder)
                    , label = theLabel
                    }


button : String -> msg -> List (Attribute msg) -> Element msg
button buttonText msg layout =
    row (List.concat [ box, layout, [ onClick msg ] ])
        [ El.el [ centerX, centerY ] (El.text buttonText) ]


checkbox : String -> (Bool -> msg) -> Bool -> Element msg
checkbox labelText msg value =
    Input.checkbox []
        { onChange = Just msg
        , icon = Nothing
        , checked = value
        , label = Input.labelRight [] (El.text labelText)
        }
