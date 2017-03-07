module Form.FormView exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, select, option, label, p)
-- import Html.Attributes exposing (..)
import Models exposing (Model)
import Form.Models exposing (..)
import Messages exposing (Msg(..))
import Material
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Grid exposing(..)


formView : Model -> Html Msg
formView model =
    case model.form of
        Nothing ->
            text "Fel! inget formulär hittades"

        Just form ->
            div []
                (List.map (\formStep -> formStepView formStep model.mdl) form.formSteps)


formStepView : FormStep -> Material.Model -> Html Msg
formStepView formStep mdl =
    let
        questionViews =
            List.map (\question -> div [] [ (questionView question mdl) ]) formStep.questions
    in
        div []
            ([ h3 [] [ text formStep.stepTitle ] ] ++ questionViews)


questionView : Question -> Material.Model -> Html Msg
questionView question mdl =
    let
        questionText =
            text question.questionText

        questionControl =
            case question.questionType of
                TextType ->
                    (qTextView question mdl)

                TextType_email ->
                    (qTextEmailView question mdl)

                ChoiceType ->
                    qChoiceView question mdl

                --text ("text: " ++ question.questionText)
                InfoType ->
                    text ("text: " ++ question.questionText)

                NoType ->
                    text "Unknown question type"
    in
        div
            []
            [ div [] [ questionText ]
            , questionControl
            ]


qTextView : Question -> Material.Model -> Html Msg
qTextView question mdl =
    Textfield.render Mdl
        [ question.questionIndex ]
        mdl
        [ Textfield.label question.questionText
        , Textfield.floatingLabel
        , Textfield.text_
        ]
        []


qTextEmailView : Question -> Material.Model -> Html Msg
qTextEmailView question mdl =
    Textfield.render Mdl
        [ question.questionIndex ]
        mdl
        [ Textfield.label question.questionText
        , Textfield.floatingLabel
        , Textfield.email
        ]
        []


qInfoView : Question -> Material.Model -> Html Msg
qInfoView question mdl =
    p [] [ text question.questionText ]


qChoiceView : Question -> Material.Model -> Html Msg
qChoiceView question mdl =
    let
        choiceToggles =
            List.map (\choice -> qOptionView choice question.questionText mdl) question.choices

        choiceToggelRows =
            List.map (\choiceToggle -> cell [ size All 6 ] [ choiceToggle ]) choiceToggles
    in
        grid
            []
            choiceToggelRows


qOptionView : Choice -> String -> Material.Model -> Html Msg
qOptionView choice groupName mdl =
    Toggles.radio Mdl
        [ choice.choiceIndex ]
        mdl
        [ Toggles.value False
        , Toggles.group groupName
        , Toggles.ripple
          --   , Options.onToggle MyRadioMsg1
        ]
        [ text choice.choiceText ]
