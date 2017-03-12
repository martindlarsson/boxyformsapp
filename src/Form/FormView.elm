module Form.FormView exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, select, option, label, p)
import Models exposing (Model)
import Form.Models exposing (..)
import Messages exposing (Msg(..))
import Helpers exposing(..)
import Material
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Options as Options
import Material.Button as Button
import Material.Grid exposing (..)


type alias QuestionModel =
    { question : Question
    , answer : Answer
    , mdl : Material.Model
    }


formView : Model -> Html Msg
formView model =
    case model.form of
        Nothing ->
            text "Laddar in formuläret, snart så..."

        -- Just emptyForm ->
        --     text "Inget formulär hittade med detta id. Det verkar blivit något fel tyvärr."

        Just form ->
            let
                currentStep = findFormStep model.currentFormStepId form.formSteps

                indexCurrentStep =
                    case currentStep of
                        Nothing ->
                            -1 -- TODO, hantera detta!

                        Just currentStep ->
                            indexOf form.formSteps currentStep

                numberOfSteps = List.length form.formSteps
            in
                div []
                    [ formStepView (findFormStep model.currentFormStepId form.formSteps) model.answers model.mdl
                    , grid [] (formButtonView model indexCurrentStep numberOfSteps)
                    ]



formButtonView : Model -> Int -> Int -> List (Cell Msg)
formButtonView model indexCurrentStep numberOfSteps =
    if (indexCurrentStep == 0) && (numberOfSteps > 1) then
        [ cell [ size All 12 ] [ nextButton model ]  ] --Första steget av många [Next]
    else if (indexCurrentStep == 0) && ((indexCurrentStep + 1) == numberOfSteps) then
        [ cell [ size All 12 ] [ payButton model ] ] --Första och sista steget [Betala]"
    else if (indexCurrentStep + 1) < numberOfSteps then
        [ cell [ size All 6 ] [ prevButton model ] -- "[Previouse] mitt i [Betala?]"
        , cell [ size All 6 ] [ payButton model ]
        ]
    else if (indexCurrentStep + 1) == numberOfSteps then
        [ cell [ size All 6 ] [ prevButton model ] -- [Previouse] Sista steget [Betala?]
        , cell [ size All 6 ] [ payButton model ]
        ]
    else
        [ cell [ size All 12 ] [ text "Något verkar ha gått snett..."] ]


nextButton : Model -> Html Msg
nextButton model =
    Button.render Mdl [0] model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Button.onClick FormNextButtonClicked
        ]
        [ text "Nästa" ]


prevButton : Model -> Html Msg
prevButton model =
    Button.render Mdl [1] model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        -- , Button.onClick FormPreviouseButtonClicked
        ]
        [ text "Föregående" ]


payButton : Model -> Html Msg
payButton model =
    Button.render Mdl [2] model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        -- , Button.onClick FormPayButtonClicked
        ]
        [ text "Betala" ]



formStepView : Maybe FormStep -> List Answer -> Material.Model -> Html Msg
formStepView formStep answers mdl =
        case formStep of
            Nothing -> text "Formuläret innehåller inga frågor. Något verkar vara på tok!"

            Just formStep ->
                let
                    questionViews =
                        List.map (\question -> div [] [ (questionView question (findAnswer question.questionId answers) mdl) ]) formStep.questions
                in
                    div []
                        ([ h3 [] [ text formStep.stepTitle ] ] ++ questionViews)


questionView : Question -> Answer -> Material.Model -> Html Msg
questionView question answer mdl =
    let
        questionModel = QuestionModel question answer mdl

        questionText =
            label [] [ text question.questionText ]

        questionControl =
            case question.questionType of
                TextType ->
                    qTextView questionModel

                TextType_email ->
                    qTextEmailView questionModel

                ChoiceType ->
                    qChoiceView questionModel

                InfoType ->
                    qInfoView questionModel

                NoType ->
                    text "Unknown question type"
    in
        div
            []
            [ div [] [ questionText ]
            , questionControl
            ]


qTextView : QuestionModel -> Html Msg
qTextView questionModel =
    let
        question = questionModel.question

        answer = questionModel.answer
    in
        Textfield.render Mdl
            [ question.questionIndex ]
            questionModel.mdl
            [ Textfield.text_
            , Options.onInput (SetAnswer question.questionId)
            ]
            [ text answer.answer ]


qTextEmailView : QuestionModel -> Html Msg
qTextEmailView questionModel =
    let
        question = questionModel.question

        answer = questionModel.answer
    in
        Textfield.render Mdl
            [ question.questionIndex ]
            questionModel.mdl
            [ Textfield.email
            , Options.onInput (SetAnswer question.questionId)
            ]
            [ text answer.answer ]


qInfoView : QuestionModel -> Html Msg
qInfoView questionModel =
    let
        question = questionModel.question
    in
        p [] [ text question.questionText ]


qChoiceView : QuestionModel -> Html Msg
qChoiceView questionModel =
    let
        question = questionModel.question

        answer = questionModel.answer

        choiceToggles =
            List.map (\choice -> qOptionView choice questionModel) question.choices

        choiceToggelRows =
            List.map (\choiceToggle -> cell [ size All 12 ] [ choiceToggle ]) choiceToggles
    in
        grid
            []
            choiceToggelRows


qOptionView : Choice -> QuestionModel -> Html Msg
qOptionView choice questionModel =
    let
        answer = questionModel.answer

        question = questionModel.question

        toggleValue =
            answer.answer == choice.choiceText
    in
        Toggles.radio Mdl
            [ choice.choiceIndex ]
            questionModel.mdl
            [ Toggles.value toggleValue
            , Toggles.group question.questionText
            , Toggles.ripple
            , Options.onToggle (SetAnswer question.questionId choice.choiceText)
            ]
            [ text choice.choiceText ]
