module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form exposing (..)
import FeatherIcons


-- Msg --


type Msg
    = SaveForm
    | TextChanged Field String
    | CheckboxChanged Field Bool
    | AddQuestion QuestionType
    | AddQuestionControlHover ControlIndex
    | AddQuestionControlNoHover
    | DummyMessage


type Field
    = NoField
    | FormName
    | Description
    | DateFrom
    | DateTo
    | Public
    | FormField


type QuestionType
    = Info
    | Text
    | Choice
    | YesNo


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


update : Msg -> Model -> ( Model, Cmd msg )
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

            AddQuestion qType ->
                ( updateAddQuestion qType model, Cmd.none )

            AddQuestionControlHover controlIndex ->
                ( { model | controlHoverState = Hovering controlIndex }, Cmd.none )

            AddQuestionControlNoHover ->
                ( { model | controlHoverState = NoControl }, Cmd.none )

            DummyMessage ->
                ( model, Cmd.none )


updateAddQuestion : QuestionType -> Model -> Model
updateAddQuestion questionType model =
    model



-- VIEW --


view : Model -> Element Styles variation Msg
view model =
    column
        None
        [ spacing 20 ]
        ((formMetadataView model)
            ++ (questionsView model)
            ++ [ questionView { id = "", questionText = "textfråga", questionType = TextType } 1 ]
            ++ [ questionView { id = "", questionText = "infofråga", questionType = InfoType } 1 ]
            ++ [ questionView { id = "", questionText = "val", questionType = (ChoiceType []) } 1 ]
            ++ [ questionView { id = "", questionText = "ja eller nej", questionType = YesNoType } 1 ]
            ++ [ FormView.button "Spara" SaveForm [] ]
        )


formMetadataView : Model -> List (Element Styles variation Msg)
formMetadataView model =
    let
        userState =
            model.userState

        form =
            model.form

        userForm =
            case (userState) of
                UserIsOK ->
                    empty

                NotLoggedIn ->
                    empty

                UserNeedsMoreInfo ->
                    FormView.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret."
    in
        [ paragraph H2 [] [ Element.text "Skapa nytt formulär" ]
        , paragraph None [] [ Element.text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
        , userForm
        , FormView.textInput Singleline "Namn" "Namnet på formuläret" form.name (TextChanged FormName) Enabled
        , FormView.textInput Multiline "Beskrivning" "Beskriv syftet med formuläret" form.description (TextChanged Description) Enabled
        , FormView.textInput Singleline "Från-datum" "Från detta datum kan användare fylla i formuläret" form.dateFrom (TextChanged DateFrom) Enabled
        , FormView.textInput Singleline "Till-datum" "Till och med detta datum kan användare fylla i formuläret" form.dateTo (TextChanged DateTo) Enabled
        , FormView.checkbox "Publikt formulär" (CheckboxChanged Public) form.public
        ]


questionsView : Model -> List (Element Styles variation Msg)
questionsView model =
    [ column QuestionsView
        [ onMouseEnter (AddQuestionControlHover 0), onMouseLeave AddQuestionControlNoHover ]
        [ addQuestionView model 0 ]
    ]


addQuestionView : Model -> ControlIndex -> Element Styles variation Msg
addQuestionView model index =
    let
        plus =
            [ addQuestionButton FeatherIcons.plusCircle "Lägg till..." (AddQuestion Info) ]

        allControls =
            [ addQuestionButton FeatherIcons.alignJustify "Text" (AddQuestion Text)
            , addQuestionButton FeatherIcons.chevronDown "Lista med alternativ" (AddQuestion Choice)
            , addQuestionButton FeatherIcons.checkCircle "Ja/Nej" (AddQuestion YesNo)
            , addQuestionButton FeatherIcons.info "Information" (AddQuestion Info)
            ]

        controls =
            case (model.controlHoverState) of
                NoControl ->
                    plus

                Hovering controlIndex ->
                    if (controlIndex == index) then
                        allControls
                    else
                        plus
    in
        row AddQuestionsView
            [ center, spacing 30, padding 5, verticalCenter ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
addQuestionButton icon titleText msg =
    Element.el AddQuestionButton
        [ onClick msg ]
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
