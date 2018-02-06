module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Color
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form exposing (..)
import FeatherIcons
import Array exposing (..)
import Random exposing (..)
import Html.Attributes exposing (title)
import Util exposing (..)


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
    | MoveQuestionUp QuestionIndex
    | MoveQuestionDown QuestionIndex
    | RemoveQuestion QuestionIndex


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
    , device : Device
    }


type ControlHover
    = NoControl
    | Hovering ControlIndex


type alias ControlIndex =
    Int


type alias QuestionIndex =
    Int


init : User -> Device -> Model
init user device =
    { userState = validateUser (Just user)
    , form = emptyForm user
    , controlHoverState = NoControl
    , device = device
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

            MoveQuestionUp questionIndex ->
                let
                    updatedQuesions =
                        moveItemUp questionIndex model.form.questions
                in
                    ( { model | form = { oldForm | questions = updatedQuesions } }, Cmd.none )

            MoveQuestionDown questionIndex ->
                ( model, Cmd.none )

            RemoveQuestion questionIndex ->
                let
                    oldQuestions =
                        model.form.questions

                    maybyQuestionToRemove =
                        Array.get questionIndex oldQuestions

                    newQuestions =
                        case maybyQuestionToRemove of
                            Nothing ->
                                oldQuestions

                            Just questionToRemove ->
                                Array.filter (\q -> q.id /= questionToRemove.id) oldQuestions
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
                in
                    Array.set questionIndex newQuestion oldQuestions


updateAddQuestion : QuestionType -> ControlIndex -> Array Question -> Array Question
updateAddQuestion qType index oldQuestions =
    let
        inArrayPosition =
            getArrayPosition index oldQuestions

        questionToInsert =
            emptyQuestion qType
    in
        insertItemIntoArray questionToInsert index oldQuestions


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


view : Model -> Element Msg
view model =
    let
        form =
            model.form
    in
        column
            [ spacing 10 ]
            [ formMetadataView model
            , questionsView model
            , FormView.button "Spara" SaveForm []
            ]


formMetadataView : Model -> Element Msg
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
        column
            [ spacing 10 ]
            [ paragraph h1 [ Element.text "Skapa nytt formulär" ]
            , paragraph [] [ Element.text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
            , userForm
            , FormView.textInput Singleline "Namn" "Namnet på formuläret" form.name (TextChanged FormName) Enabled
            , FormView.textInput Multiline "Beskrivning" "Beskriv syftet med formuläret" form.description (TextChanged Description) Enabled
            , FormView.textInput Singleline "Från-datum" "Från detta datum kan användare fylla i formuläret" form.dateFrom (TextChanged DateFrom) Enabled
            , FormView.textInput Singleline "Till-datum" "Till och med detta datum kan användare fylla i formuläret" form.dateTo (TextChanged DateTo) Enabled
            , FormView.checkbox "Publikt formulär" (CheckboxChanged Public) form.public
            ]


questionsView : Model -> Element Msg
questionsView model =
    let
        form =
            model.form

        questionViews =
            Array.map (\q -> (questionTuple model.controlHoverState q (getItemIndex form.questions q) model.device)) form.questions
    in
        column BoxyStyle.questionView
            ([ addQuestionView model.controlHoverState 0 model.device ]
                ++ (Array.toList questionViews)
            )


questionTuple : ControlHover -> Question -> Maybe Int -> Device -> Element Msg
questionTuple hoverState question maybeIndex device =
    case maybeIndex of
        Nothing ->
            Element.empty

        Just index ->
            column
                []
                [ questionView question index
                , addQuestionView hoverState (index + 1) device
                ]


addQuestionView : ControlHover -> ControlIndex -> Device -> Element Msg
addQuestionView hoverState index device =
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
            case device.phone of
                True ->
                    allControls

                False ->
                    case (hoverState) of
                        NoControl ->
                            plus

                        Hovering controlIndex ->
                            if (controlIndex == index) then
                                allControls
                            else
                                plus
    in
        Element.row
            [ Font.size 40
            , Font.color Color.darkGray
            , center
            , spacingXY 30 0
            , padding 5
            , centerY
            , onMouseEnter (AddQuestionControlHover index)
            , onMouseLeave AddQuestionControlNoHover
            ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Msg
addQuestionButton icon titleText msg =
    Element.column
        [ centerY, center ]
        [ Element.el
            [ onClick msg ]
            --, Element.Attributes.toAttr (Html.Attributes.title titleText)
            (Element.html
                (icon |> FeatherIcons.toHtml [])
            )
        , Element.el [ Font.size 10, Font.color Color.darkGray ] (Element.text titleText)
        ]


questionView : Question -> Int -> Element Msg
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
        Element.row
            [ Background.color Color.lightOrange
            , spacing 10
            , padding 10
            , Border.shadow
                { offset = ( 5, 5 )
                , size = 3
                , blur = 20
                , color = Color.darkGray
                }
            ]
            [ Element.column [ width fill ] [ questionContent ]
            , Element.column [ width (px 50) ] [ questionButtons questionIndex ]
            ]



-- { columns = [ fill, (px 50) ]
-- , rows = [ fill ]
-- , cells =
--     [ Element.cell
--         { start = ( 0, 0 )
--         , width = 1
--         , height = 1
--         , content = questionContent
--         }
--     , Element.cell
--         { start = ( 1, 0 )
--         , width = 1
--         , height = 1
--         , content = questionButtons questionIndex
--         }
--     ]
-- }


infoQuestion : Question -> Element Msg
infoQuestion question =
    FormView.textInput
        Multiline
        "Informationstext"
        "Här kan du skriva en informativ text som hjälper användaren."
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


textQuestion : Question -> Element Msg
textQuestion question =
    FormView.textInput
        Multiline
        "Textfråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


yesNoQuestion : Question -> Element Msg
yesNoQuestion question =
    FormView.textInput
        Multiline
        "Ja/Nej-fråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


choiceQuestion : Question -> Element Msg
choiceQuestion question =
    FormView.textInput
        Multiline
        "Flervalsfråga"
        "Frågetext"
        question.questionText
        (UpdateQuestionText question.id)
        Enabled


questionButtons : QuestionIndex -> Element Msg
questionButtons qIndex =
    Element.column
        [ center, centerY, padding 5, spacing 10 ]
        [ (iconButton FeatherIcons.arrowUp "Flytta upp" (MoveQuestionUp qIndex))
        , (iconButton FeatherIcons.trash2 "Radera" (RemoveQuestion qIndex))
        , (iconButton FeatherIcons.arrowDown "Flytta ned" (MoveQuestionDown qIndex))
        ]


iconButton : FeatherIcons.Icon -> String -> Msg -> Element Msg
iconButton icon titleText msg =
    Element.el
        [ Font.color Color.lightCharcoal
        , onClick msg

        -- , hover
        --     [ Color.text Color.charcoal
        --     , cursor "pointer"
        --     ]
        ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )
