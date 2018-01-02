module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form exposing (..)
import FeatherIcons
import Array exposing (..)
import Random exposing (..)
import Html.Attributes exposing (title)


-- Msg --


type Msg
    = SaveForm
    | TextChanged Field String
    | CheckboxChanged Field Bool
    | AddQuestion QuestionType ControlIndex
    | AddQuestionControlHover ControlIndex
    | AddQuestionControlNoHover
    | DummyMessage
    | NewRandomId Int
    | UpdateQuestionText QuestionId String


type Field
    = NoField
    | FormName
    | Description
    | DateFrom
    | DateTo
    | Public
    | FormField


type alias Model =
    { form : Form
    , userState : UserState
    , controlHoverState : ControlHover
    }


type ControlHover
    = NoControl
    | Hovering ControlIndex


type alias ControlIndex =
    Int


init : User -> Model
init user =
    { userState = validateUser (Just user)
    , form = emptyForm user
    , controlHoverState = NoControl
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldForm =
            model.form
    in
        case msg of
            SaveForm ->
                ( model, Cmd.none )

            TextChanged NoField value ->
                ( model, Cmd.none )

            TextChanged FormName value ->
                ( { model | form = { oldForm | name = value } }, Cmd.none )

            TextChanged Description value ->
                ( { model | form = { oldForm | description = value } }, Cmd.none )

            TextChanged DateFrom value ->
                ( { model | form = { oldForm | dateFrom = value } }, Cmd.none )

            TextChanged DateTo value ->
                ( { model | form = { oldForm | dateTo = value } }, Cmd.none )

            TextChanged _ value ->
                ( model, Cmd.none )

            CheckboxChanged Public value ->
                ( { model | form = { oldForm | public = value } }, Cmd.none )

            CheckboxChanged _ value ->
                ( model, Cmd.none )

            AddQuestion qType index ->
                ( { model | form = { oldForm | questions = (updateAddQuestion qType index model.form.questions) } }, Random.generate NewRandomId (Random.int Random.minInt Random.maxInt) )

            AddQuestionControlHover controlIndex ->
                ( { model | controlHoverState = Hovering controlIndex }, Cmd.none )

            AddQuestionControlNoHover ->
                ( { model | controlHoverState = NoControl }, Cmd.none )

            NewRandomId newId ->
                ( { model | form = { oldForm | questions = (updateQuestionId newId model.form.questions) } }, Cmd.none )

            DummyMessage ->
                ( model, Cmd.none )

            UpdateQuestionText questionId questionText ->
                let
                    questions =
                        model.form.questions

                    newQuestions =
                        updateQuestionText questions questionId questionText
                in
                    ( { model | form = { oldForm | questions = newQuestions } }, Cmd.none )


updateQuestionText : Array Question -> QuestionId -> String -> Array Question
updateQuestionText oldQuestions questionId newQuestionString =
    let
        maybeQuestionTuple =
            getItemWitId oldQuestions questionId 0
    in
        case maybeQuestionTuple of
            Nothing ->
                oldQuestions

            Just ( question, questionIndex ) ->
                let
                    newQuestion =
                        { question | questionText = newQuestionString }

                    _ =
                        Debug.log "updateQuestionText" newQuestion
                in
                    Array.set questionIndex newQuestion oldQuestions


type InArrayPosition
    = First
    | Middle
    | Last


updateAddQuestion : QuestionType -> ControlIndex -> Array Question -> Array Question
updateAddQuestion qType index oldQuestions =
    let
        arrayLength =
            Array.length oldQuestions

        inArrayPosition =
            if (index == 0) then
                First
            else if (arrayLength == index) then
                Last
            else
                Middle

        _ =
            Debug.log "updateAddQuestion" ("arrayLenth: " ++ (toString arrayLength) ++ ", index: " ++ (toString index) ++ " position: " ++ (toString inArrayPosition))
    in
        case inArrayPosition of
            First ->
                Array.append (Array.fromList [ emptyQuestion qType ]) oldQuestions

            Middle ->
                let
                    firstHalf =
                        Array.slice 0 index oldQuestions

                    secondHalf =
                        Array.slice index arrayLength oldQuestions

                    newFirstHalf =
                        Array.append firstHalf (fromList [ emptyQuestion qType ])

                    newArray =
                        Array.append newFirstHalf secondHalf

                    -- _ =
                    --     Debug.log "Add q middle, firstHalf" firstHalf
                    -- _ =
                    --     Debug.log "Add q middle, secondHalf" secondHalf
                in
                    newArray

            Last ->
                Array.push (emptyQuestion qType) oldQuestions


updateQuestionId : Int -> Array Question -> Array Question
updateQuestionId newRandomId questions =
    let
        maybeOldQuestionTuple =
            getItemWitId questions "new" 0
    in
        case maybeOldQuestionTuple of
            Nothing ->
                questions

            Just ( oldQuestion, index ) ->
                let
                    newQuestion =
                        { oldQuestion | id = toString newRandomId }
                in
                    Array.set index newQuestion questions



-- VIEW --


view : Model -> Element Styles variation Msg
view model =
    let
        form =
            model.form
    in
        column
            None
            [ spacing 10 ]
            [ formMetadataView model
            , questionsView model.controlHoverState form.questions
            , FormView.button "Spara" SaveForm []
            ]


formMetadataView : Model -> Element Styles variation Msg
formMetadataView model =
    let
        userState =
            model.userState

        form =
            model.form

        userForm =
            case (userState) of
                UserIsOK ->
                    Element.empty

                NotLoggedIn ->
                    Element.empty

                UserNeedsMoreInfo ->
                    FormView.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret."
    in
        column FormMetadataView
            [ spacing 10 ]
            [ paragraph H1 [] [ Element.text "Skapa nytt formulär" ]
            , paragraph None [] [ Element.text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
            , userForm
            , FormView.textInput Singleline "Namn" "Namnet på formuläret" form.name (TextChanged FormName) Enabled
            , FormView.textInput Multiline "Beskrivning" "Beskriv syftet med formuläret" form.description (TextChanged Description) Enabled
            , FormView.textInput Singleline "Från-datum" "Från detta datum kan användare fylla i formuläret" form.dateFrom (TextChanged DateFrom) Enabled
            , FormView.textInput Singleline "Till-datum" "Till och med detta datum kan användare fylla i formuläret" form.dateTo (TextChanged DateTo) Enabled
            , FormView.checkbox "Publikt formulär" (CheckboxChanged Public) form.public
            ]


questionsView : ControlHover -> Array Question -> Element Styles variation Msg
questionsView hoverState questions =
    let
        questionViews =
            Array.map (\q -> (questionTuple hoverState q (getItemIndex questions q 1))) questions
    in
        column QuestionsView
            []
            ([ addQuestionView hoverState 0 ]
                ++ (Array.toList questionViews)
            )


questionTuple : ControlHover -> Question -> Maybe Int -> Element Styles variation Msg
questionTuple hoverState question maybeIndex =
    case maybeIndex of
        Nothing ->
            Element.empty

        Just index ->
            column None
                []
                [ questionView question index
                , addQuestionView hoverState index
                ]


addQuestionView : ControlHover -> ControlIndex -> Element Styles variation Msg
addQuestionView hoverState index =
    let
        plus =
            [ addQuestionButton FeatherIcons.plusCircle "Lägg till fråga" (AddQuestion InfoType index) ]

        allControls =
            [ addQuestionButton FeatherIcons.alignJustify "Text" (AddQuestion TextType index)
            , addQuestionButton FeatherIcons.chevronDown "Val" (AddQuestion (ChoiceType []) index)
            , addQuestionButton FeatherIcons.checkCircle "Ja/Nej" (AddQuestion YesNoType index)
            , addQuestionButton FeatherIcons.info "Info" (AddQuestion InfoType index)
            ]

        controls =
            case (hoverState) of
                NoControl ->
                    plus

                Hovering controlIndex ->
                    if (controlIndex == index) then
                        allControls
                    else
                        plus
    in
        row AddQuestionsView
            [ center, spacingXY 30 0, padding 5, verticalCenter, onMouseEnter (AddQuestionControlHover index), onMouseLeave AddQuestionControlNoHover ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
addQuestionButton icon titleText msg =
    Element.column None
        [ verticalCenter, center ]
        [ Element.el AddQuestionButton
            [ onClick msg, Element.Attributes.toAttr (Html.Attributes.title titleText) ]
            (Element.html
                (icon |> FeatherIcons.toHtml [])
            )
        , Element.el AddQuestionsSubView [] (Element.text titleText)
        ]


questionView : Question -> Int -> Element Styles variation Msg
questionView question questionIndex =
    let
        questionType =
            question.questionType

        questionContent =
            case questionType of
                TextType ->
                    textQuestion question

                InfoType ->
                    infoQuestion question

                ChoiceType choices ->
                    choiceQuestion question

                YesNoType ->
                    yesNoQuestion question
    in
        Element.grid QuestionView
            [ spacing 10, padding 10 ]
            { columns = [ fill, (px 50) ]
            , rows = [ fill ]
            , cells =
                [ Element.cell
                    { start = ( 0, 0 )
                    , width = 1
                    , height = 1
                    , content = questionContent
                    }
                , Element.cell
                    { start = ( 1, 0 )
                    , width = 1
                    , height = 1
                    , content = questionButtons questionIndex
                    }
                ]
            }


infoQuestion : Question -> Element Styles variation Msg
infoQuestion question =
    FormView.textInput
        Multiline
        "Informationstext"
        "Här kan du skriva en informativ text som hjälper användaren."
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


textQuestion : Question -> Element Styles variation Msg
textQuestion question =
    FormView.textInput
        Multiline
        "Textfråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


yesNoQuestion : Question -> Element Styles variation Msg
yesNoQuestion question =
    FormView.textInput
        Multiline
        "Ja/Nej-fråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


choiceQuestion : Question -> Element Styles variation Msg
choiceQuestion question =
    FormView.textInput
        Multiline
        "Flervalsfråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


questionButtons : Int -> Element Styles variation Msg
questionButtons questionIndex =
    Element.column None
        [ center, verticalCenter, padding 5, spacing 10 ]
        [ (iconButton FeatherIcons.arrowUp "Flytta upp" DummyMessage)
        , (iconButton FeatherIcons.trash2 "Radera" DummyMessage)
        , (iconButton FeatherIcons.arrowDown "Flytta ned" DummyMessage)
        ]


iconButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
iconButton icon titleText msg =
    Element.el IconButton
        [ onClick msg ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )



-- iconButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
-- iconButton icon titleText msg =
--     column None
--         [ verticalCenter, height (px 300) ]
--         [ Element.el IconButton
--             [ onClick msg ]
--             (Element.html
--                 (icon |> FeatherIcons.toHtml [])
--             )
--         , Element.text titleText
--         ]
