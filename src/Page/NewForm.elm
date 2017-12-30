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


type Field
    = NoField
    | FormName
    | Description
    | DateFrom
    | DateTo
    | Public
    | FormField



-- type QuestionType
--     = Info
--     | Text
--     | Choice
--     | YesNo


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


updateAddQuestion : QuestionType -> ControlIndex -> Array Question -> Array Question
updateAddQuestion qType index oldQuestions =
    let
        arrayLength =
            Array.length oldQuestions

        firstHalf =
            Array.slice 0 index oldQuestions

        secondHalf =
            Array.slice index arrayLength oldQuestions

        newFirstHalf =
            Array.append firstHalf (fromList [ emptyQuestion qType ])

        newArray =
            Array.append newFirstHalf secondHalf
    in
        newArray


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
            []
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
        column None
            [ spacing 10 ]
            [ paragraph H2 [] [ Element.text "Skapa nytt formulär" ]
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
            Array.map (\q -> (questionTuple hoverState q (getItemIndex questions q 0))) questions
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
        nextIndex =
            index + 1

        plus =
            [ addQuestionButton FeatherIcons.plusCircle "Lägg till..." (AddQuestion InfoType nextIndex) ]

        allControls =
            [ addQuestionButton FeatherIcons.alignJustify "Text" (AddQuestion TextType nextIndex)
            , addQuestionButton FeatherIcons.chevronDown "Lista med alternativ" (AddQuestion (ChoiceType []) nextIndex)
            , addQuestionButton FeatherIcons.checkCircle "Ja/Nej" (AddQuestion YesNoType nextIndex)
            , addQuestionButton FeatherIcons.info "Information" (AddQuestion InfoType nextIndex)
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
            [ center, spacing 30, padding 5, verticalCenter, onMouseEnter (AddQuestionControlHover index), onMouseLeave AddQuestionControlNoHover ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
addQuestionButton icon titleText msg =
    Element.el AddQuestionButton
        [ onClick msg, Element.Attributes.toAttr (Html.Attributes.title titleText) ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )


questionView : Question -> Int -> Element Styles variation Msg
questionView question questionIndex =
    let
        questionType =
            question.questionType

        questionContent =
            case questionType of
                TextType ->
                    infoQuestion question

                InfoType ->
                    infoQuestion question

                ChoiceType choices ->
                    infoQuestion question

                YesNoType ->
                    infoQuestion question
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
        (TextChanged FormField)
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
