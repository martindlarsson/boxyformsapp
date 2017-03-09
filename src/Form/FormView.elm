module Form.FormView exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, select, option, label, p)
-- import Html.Attributes exposing (..)
import Models exposing (Model)
import Form.Models exposing (..)
import Messages exposing (Msg(..))
import Material
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Options as Options
import Material.Grid exposing(..)


formView : Model -> Html Msg
formView model =
    case model.form of
        Nothing ->
            text "Fel! inget formulär hittades"

        Just form ->
            div []
                (List.map (\formStep -> formStepView formStep model.answers model.mdl) form.formSteps)


formStepView : FormStep -> Maybe (List Answer) -> Material.Model -> Html Msg
formStepView formStep answers mdl =
    let
        questionViews =
            List.map (\question -> div [] [ (questionView question (findAnswer question.questionId answers) mdl) ]) formStep.questions
    in
        div []
            ([ h3 [] [ text formStep.stepTitle ] ] ++ questionViews)


findAnswer : QuestionId -> Maybe (List Answer) -> Maybe Answer
findAnswer qId maybeAnswers =
    case maybeAnswers of
            Nothing -> Nothing

            Just answers -> 
                answers
                    |> List.filter (\answer -> answer.questionId == qId)
                    |> List.head


questionView : Question -> Maybe Answer -> Material.Model -> Html Msg
questionView question answer mdl =
    let
        questionText =
            label [] [ text question.questionText ]

        questionControl =
            case question.questionType of
                TextType ->
                    (qTextView question mdl)

                TextType_email ->
                    (qTextEmailView question mdl)

                ChoiceType ->
                    qChoiceView question answer mdl 

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
        [ --Textfield.text_ TODO, lägg in eventuellt svar om anv backar i formuläret
        Options.onInput (SetAnswer question.questionId)
        ]
        []


qTextEmailView : Question -> Material.Model -> Html Msg
qTextEmailView question mdl =
    Textfield.render Mdl
        [ question.questionIndex ]
        mdl
        [ --Textfield.label question.questionText
        -- , Textfield.floatingLabel
        Textfield.email
        , Options.onInput (SetAnswer question.questionId)
        ]
        []


qInfoView : Question -> Material.Model -> Html Msg
qInfoView question mdl =
    p [] [ text question.questionText ]


qChoiceView : Question -> Maybe Answer -> Material.Model -> Html Msg
qChoiceView question maybeAnswer mdl =
    let
        choiceToggles =
            List.map (\choice -> qOptionView choice question.questionId maybeAnswer question.questionText mdl) question.choices

        choiceToggelRows =
            List.map (\choiceToggle -> cell [ size All 12 ] [ choiceToggle ]) choiceToggles
    in
        grid
            []
            choiceToggelRows


qOptionView : Choice -> QuestionId -> Maybe Answer -> String -> Material.Model -> Html Msg
qOptionView choice qusetionId maybeAnswer groupName mdl =
    let
      toggleValue =
        case maybeAnswer of
            Nothing -> False

            Just answer -> answer.answer == choice.choiceText
    in
        Toggles.radio Mdl
            [ choice.choiceIndex ]
            mdl
            [ Toggles.value toggleValue
            , Toggles.group groupName
            , Toggles.ripple
            , Options.onToggle (SetAnswer qusetionId choice.choiceText)
            ]
            [ text choice.choiceText ]
