module Form.FormView exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, select, option, label, p)
import Html.Attributes exposing (class)
import Models exposing (Model)
import Form.Models as Model exposing (..)
import Messages exposing (Msg(..))
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
        NoForm ->
            grid [] [ cell [ size All 6 ] [ text "Inget formulär har valts" ] ]

        LoadingForm ->
            grid [] [ cell [ size All 6 ] [ text "Laddar in formuläret, snart så..." ] ]

        ErrorLoadingForm errMsg ->
            grid [] [ cell [ size All 6 ] [ text ("Fel vid laddning av formulär. " ++ errMsg) ] ]

        FormLoaded form ->
            let
                formStepState =
                    getFormStepState form.formSteps

                currentStep =
                    getCurrentStep form.formSteps

                _ =
                    Debug.log "loading formView, answers" model.answers
            in
                div [ class "mdl-grid" ]
                    [ div [ class "mdl-layout-spacer" ] []
                    , div [ class "mdl-cell mdl-cell--8-col" ] [ grid [] ((formStepView currentStep model.answers model.mdl) ++ (formButtonView model formStepState)) ]
                    , div [ class "mdl-layout-spacer" ] []
                    ]


formButtonView : Model -> FormStepState -> List (Cell Msg)
formButtonView model formStepState =
    case formStepState of
        HasOneStep ->
            [ cell [ size All 12 ] [ payButton model ] ]

        HasPrevButNoNext ->
            [ cell [ size All 11 ] [ prevButton model ]
            , cell [ size All 1 ] [ payButton model ]
            ]

        HasNoPrevButNext ->
            [ cell [ size All 11 ] []
            , cell [ size All 1 ] [ nextButton model ]
            ]

        HasPrevAndNext ->
            [ cell [ size All 11 ] [ prevButton model ]
            , cell [ size All 1 ] [ nextButton model ]
            ]


nextButton : Model -> Html Msg
nextButton model =
    Button.render Mdl
        [ 0 ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick FormNextButtonClicked
        ]
        [ text "Nästa" ]


prevButton : Model -> Html Msg
prevButton model =
    Button.render Mdl
        [ 1 ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple
        , Options.onClick FormPrevButtonClicked
        ]
        [ text "Föregående" ]


payButton : Model -> Html Msg
payButton model =
    Button.render Mdl
        [ 2 ]
        model.mdl
        [ Button.raised
        , Button.colored
        , Button.ripple

        -- , Button.onClick FormPayButtonClicked
        ]
        [ text "Betala" ]


formStepView : FormStep -> List Answer -> Material.Model -> List (Cell Msg)
formStepView formStep answers mdl =
    let
        questionViews =
            List.map (\question -> cell [ size All 12 ] [ (questionView question (findAnswer question.questionId answers) mdl) ]) formStep.questions
    in
        [ cell [ size All 12 ] [ h3 [] [ text formStep.stepTitle ] ] ] ++ questionViews


questionView : Question -> Answer -> Material.Model -> Html Msg
questionView question answer mdl =
    let
        questionModel =
            let
                _ =
                    Debug.log "questionView" [ (toString question.questionId), (toString answer.questionId), answer.answer ]
            in
                QuestionModel question answer mdl

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
        question =
            questionModel.question

        answer =
            questionModel.answer
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
        question =
            questionModel.question

        answer =
            questionModel.answer
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
        question =
            questionModel.question
    in
        p [] [ text question.questionText ]


qChoiceView : QuestionModel -> Html Msg
qChoiceView questionModel =
    let
        question =
            questionModel.question

        answer =
            questionModel.answer

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
        answer =
            questionModel.answer

        question =
            questionModel.question

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
