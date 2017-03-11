module Form.FormView exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, select, option, label, p)
import Models exposing (Model)
import Form.Models exposing (..)
import Messages exposing (Msg(..))
import Material
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Options as Options
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
            div []
                (List.map (\formStep -> formStepView formStep model.answers model.mdl) form.formSteps)


formStepView : FormStep -> List Answer -> Material.Model -> Html Msg
formStepView formStep answers mdl =
    let
        questionViews =
            List.map (\question -> div [] [ (questionView question (findAnswer question.questionId answers) mdl) ]) formStep.questions
    in
        div []
            ([ h3 [] [ text formStep.stepTitle ] ] ++ questionViews)


findAnswer : QuestionId -> List Answer -> Answer
findAnswer qId maybeAnswers =
    let
        maybeFoundAnswer =
            maybeAnswers
                |> List.filter (\answer -> answer.questionId == qId)
                |> List.head
    in
        case maybeFoundAnswer of
            Nothing ->
                emptyAnswer qId

            Just answer ->
                answer


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
