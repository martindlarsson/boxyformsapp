module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Keyed as Keyed exposing (..)
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
import Util exposing (..)


-- import Html.Attributes exposing (title)
-- Msg --


type Msg
    = SaveForm
    | UpdateFormMeta Field
      -- | CheckboxChanged Field Bool
    | UpdateQuestion QuestionOperation
    | AddQuestionControlHover ControlIndex
    | AddQuestionControlNoHover



-- | TextChanged
-- | AddQuestion QuestionType ControlIndex
-- | NewRandomQuestionId Int
-- | NewRandomChoiceId QuestionId Int
-- | UpdateQuestionText QuestionId String
-- | MoveQuestionUp QuestionIndex
-- | MoveQuestionDown QuestionIndex
-- | RemoveQuestion QuestionIndex
-- | ChoiceTextChanged QuestionId ChoiceIndex String
-- | UpdateChoices QuestionId ChoiceOperation


type QuestionOperation
    = AddQuestion QuestionType ControlIndex
    | UpdateQuestionId Int
    | UpdateQuestionText QuestionId String
    | MoveQuestionUp QuestionId
    | MoveQuestionDown QuestionId
    | RemoveQuestion QuestionId
    | UpdateChoice QuestionId ChoiceOperation


type ChoiceOperation
    = AddChoice
    | UpdateChoiceId ChoiceId Int
    | UpdateChoiceText ChoiceId String
    | MoveChoiceUp ChoiceId
    | MoveChoiceDown ChoiceId
    | RemoveChoice ChoiceId


type Field
    = NoField
    | FormName String
    | Description String
    | DateFrom String
    | DateTo String
    | Public Bool


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


type alias ChoiceIndex =
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

            UpdateFormMeta field ->
                ( { model | form = updateFormMetadata oldForm field }, Cmd.none )

            AddQuestionControlHover controlIndex ->
                ( { model | controlHoverState = Hovering controlIndex }, Cmd.none )

            AddQuestionControlNoHover ->
                ( { model | controlHoverState = NoControl }, Cmd.none )

            UpdateQuestion questionOperation ->
                let
                    ( newForm, command ) =
                        updateQuestion oldForm questionOperation
                in
                    ( { model | form = newForm }, command )


updateFormMetadata : Form -> Field -> Form
updateFormMetadata oldForm field =
    case field of
        NoField ->
            oldForm

        FormName value ->
            { oldForm | name = value }

        Description value ->
            { oldForm | description = value }

        DateFrom value ->
            { oldForm | dateFrom = value }

        DateTo value ->
            { oldForm | dateTo = value }

        Public value ->
            { oldForm | public = value }


updateQuestion : Form -> QuestionOperation -> ( Form, Cmd Msg )
updateQuestion oldForm qOperation =
    let
        errorReturn =
            ( oldForm, Cmd.none )
    in
        case qOperation of
            AddQuestion qType index ->
                ( addQuestion oldForm qType index
                , Random.generate (UpdateQuestion << UpdateQuestionId) (Random.int Random.minInt Random.maxInt)
                )

            -- ( { model | form = { oldForm | questions = (updateAddQuestion qType index model.form.questions) } }, Random.generate NewRandomQuestionId (Random.int Random.minInt Random.maxInt) )
            UpdateQuestionId newId ->
                ( updateQuestionId oldForm "new" newId, Cmd.none )

            UpdateQuestionText questionId newText ->
                let
                    maybeOldQuestion =
                        getQuestionWitId oldForm questionId
                in
                    case maybeOldQuestion of
                        Nothing ->
                            errorReturn

                        Just oldQuestion ->
                            let
                                newQuestion =
                                    { oldQuestion | questionText = newText }
                            in
                                ( updateFormWithQuestion oldForm questionId newQuestion, Cmd.none )

            -- ( { form | questions = updateFormWithQuestion oldForm questionId questionText }, Cmd.none )
            MoveQuestionUp questionId ->
                let
                    maybeQuestionIndex =
                        getQuestionIndex oldForm questionId
                in
                    case maybeQuestionIndex of
                        Nothing ->
                            ( oldForm, Cmd.none )

                        Just questionIndex ->
                            let
                                updatedQuesions =
                                    moveItemUp questionIndex oldForm.questions
                            in
                                ( { oldForm | questions = updatedQuesions }, Cmd.none )

            MoveQuestionDown questionId ->
                -- TODO
                errorReturn

            -- let
            --     maybeQuestionIndex =
            --         getQuestionIndex oldForm questionId
            -- in
            --     case maybeQuestionIndex of
            --         Nothing ->
            --             ( oldForm, Cmd.none )
            --         Just questionIndex ->
            --             let
            --                 updatedQuesions =
            --                     moveItemDown questionIndex oldForm.questions
            --             in
            --                 ( { oldForm | questions = updatedQuesions }, Cmd.none )
            RemoveQuestion questionId ->
                ( removeQuestion oldForm questionId, Cmd.none )

            UpdateChoice questionId choiceOperation ->
                let
                    maybeOldQuestion =
                        getQuestionWitId oldForm questionId
                in
                    case maybeOldQuestion of
                        Nothing ->
                            errorReturn

                        Just oldQuestion ->
                            case oldQuestion.questionType of
                                ChoiceType choiceList ->
                                    let
                                        ( newChoices, command ) =
                                            updateChoice choiceList choiceOperation

                                        newQuestion =
                                            { oldQuestion | questionType = ChoiceType newChoices }
                                    in
                                        ( updateFormWithQuestion oldForm oldQuestion.id newQuestion, command )

                                _ ->
                                    errorReturn


updateChoice : List Choice -> ChoiceOperation -> ( List Choice, Cmd msg )
updateChoice oldChoices choiceOp =
    case choiceOp of
        AddChoice ->
            ( List.append oldChoices [ emptyChoice ], Cmd.none )

        UpdateChoiceId choiceId newId ->
            ( oldChoices, Cmd.none )

        UpdateChoiceText choiceOd newText ->
            ( oldChoices, Cmd.none )

        MoveChoiceUp choiceIndex ->
            ( oldChoices, Cmd.none )

        MoveChoiceDown choiceIndex ->
            ( oldChoices, Cmd.none )

        RemoveChoice choiceIndex ->
            ( oldChoices, Cmd.none )


updateQuestionText : Form -> QuestionId -> String -> Form
updateQuestionText oldForm questionId newQuestionString =
    let
        maybeQuestion =
            getQuestionWitId oldForm questionId
    in
        case maybeQuestion of
            Nothing ->
                oldForm

            Just oldQuestion ->
                let
                    newQuestion =
                        { oldQuestion | questionText = newQuestionString }
                in
                    updateFormWithQuestion oldForm questionId newQuestion



-- VIEW --


view : Model -> Element Msg
view model =
    let
        form =
            model.form
    in
        Element.column
            [ spacing 20 ]
            [ formMetadataView model
            , questionsView model
            , FormView.button "Spara" SaveForm [ height (px 40) ]
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
        Element.column
            [ spacing 10 ]
            [ paragraph h1 [ Element.text "Skapa nytt formulär" ]
            , paragraph [] [ Element.text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
            , userForm
            , FormView.textInput Singleline (Just "Namn") "Namnet på formuläret" form.name (UpdateFormMeta << FormName) Enabled
            , FormView.textInput Multiline (Just "Beskrivning") "Beskriv syftet med formuläret" form.description (UpdateFormMeta << Description) Enabled
            , FormView.textInput Singleline (Just "Från-datum") "Från detta datum kan användare fylla i formuläret" form.dateFrom (UpdateFormMeta << DateFrom) Enabled
            , FormView.textInput Singleline (Just "Till-datum") "Till och med detta datum kan användare fylla i formuläret" form.dateTo (UpdateFormMeta << DateTo) Enabled
            , FormView.checkbox "Publikt formulär" (UpdateFormMeta << Public) form.public
            ]


questionsView : Model -> Element Msg
questionsView model =
    let
        questionViews =
            Array.map (\q -> (questionTuple model.controlHoverState q (getQuestionIndex model.form q.id) model.device)) model.form.questions
    in
        Element.column []
            ([ addQuestionView model.controlHoverState 0 model.device ]
                ++ (Array.toList questionViews)
            )


questionTuple : ControlHover -> Question -> Maybe QuestionIndex -> Device -> Element Msg
questionTuple hoverState question maybeIndex device =
    case maybeIndex of
        Nothing ->
            Element.empty

        Just index ->
            Element.column
                []
                [ questionView question index
                , addQuestionView hoverState (index + 1) device
                ]


addQuestionView : ControlHover -> ControlIndex -> Device -> Element Msg
addQuestionView hoverState index device =
    let
        plus =
            [ addQuestionButton FeatherIcons.plusCircle "Lägg till fråga" (UpdateQuestion (AddQuestion InfoType index)) ]

        allControls =
            [ addQuestionButton FeatherIcons.alignJustify "Text" (UpdateQuestion (AddQuestion TextType index))
            , addQuestionButton FeatherIcons.chevronDown "Val" (UpdateQuestion (AddQuestion (ChoiceType []) index))
            , addQuestionButton FeatherIcons.checkCircle "Ja/Nej" (UpdateQuestion (AddQuestion YesNoType index))
            , addQuestionButton FeatherIcons.info "Info" (UpdateQuestion (AddQuestion InfoType index))
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
            , Background.color Color.lightGray
            , center
            , spacingXY 5 10
            , padding 10
            , centerY
            , width (px 250)
            , onMouseEnter (AddQuestionControlHover index)
            , onMouseLeave AddQuestionControlNoHover
            ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Msg
addQuestionButton icon titleText msg =
    Element.column
        [ centerY, center, pointer, Font.color Color.darkGray, Font.mouseOverColor Color.lightCharcoal, onClick msg, width (px 60), padding 10 ]
        [ Element.el
            []
            --, Element.Attributes.toAttr (Html.Attributes.title titleText)
            (Element.html
                (icon |> FeatherIcons.toHtml [])
            )
        , Element.el [ Font.size 10 ] (Element.text titleText)
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

            -- , Border.shadow
            --     { offset = ( 5, 5 )
            --     , size = 3
            --     , blur = 20
            --     , color = Color.darkGray
            --     }
            ]
            [ Element.column [ width fill ] [ questionContent ]
            , Element.column [ width (px 30) ] [ questionButtons question.id ]
            ]


infoQuestion : Question -> Element Msg
infoQuestion question =
    FormView.textInput
        Multiline
        (Just "Informationstext")
        "Här kan du skriva en informativ text som hjälper användaren."
        question.questionText
        (UpdateQuestion << UpdateQuestionText question.id)
        Enabled


textQuestion : Question -> Element Msg
textQuestion question =
    FormView.textInput
        Multiline
        (Just "Textfråga")
        "Frågetext"
        question.questionText
        (UpdateQuestion << UpdateQuestionText question.id)
        Enabled


yesNoQuestion : Question -> Element Msg
yesNoQuestion question =
    FormView.textInput
        Multiline
        (Just "Ja/Nej-fråga")
        "Frågetext"
        question.questionText
        (UpdateQuestion << UpdateQuestionText question.id)
        Enabled


choiceQuestion : Question -> Element Msg
choiceQuestion question =
    let
        qType =
            question.questionType

        choiceList =
            case qType of
                ChoiceType listOfChoices ->
                    listOfChoices

                _ ->
                    []

        elementHeight =
            60 + (List.length choiceList) * 60
    in
        Element.column [ spacing 5, Font.alignLeft ]
            [ FormView.textInput
                Singleline
                (Just "Flervalsfråga")
                "Frågetext"
                question.questionText
                (UpdateQuestion << UpdateQuestionText question.id)
                Enabled
            , Element.row [ Font.alignLeft ] [ Element.text "Val" ]
            , Keyed.row
                [ Background.color Color.white
                , height (px elementHeight)
                , Border.color Color.charcoal
                , Border.width 1
                , Border.solid
                , Border.rounded 3
                , spacing 5
                ]
                [ ( "choices " ++ (toString question.id)
                  , (Element.column []
                        (List.append
                            (List.map (\c -> choiceView c question.id) choiceList)
                            [ addQuestionButton FeatherIcons.plusCircle "Lägg till val" (UpdateQuestion (UpdateChoice question.id AddChoice)) ]
                        )
                    )
                  )
                ]
            ]


choiceView : Choice -> QuestionId -> Element Msg
choiceView choice questionId =
    Element.row
        [ Background.color Color.lightGrey
        , height (px 60)
        , Border.color Color.white
        , Border.width 3
        , Border.solid
        , Border.rounded 3
        , padding 5
        ]
        [ Element.column [ width fill, centerY ]
            [ FormView.textInput Singleline Nothing "inget..." choice.choiceText (\text -> (UpdateQuestion (UpdateChoice questionId (UpdateChoiceText choice.id text)))) Enabled ]
        , Element.column [ width (px 100), centerY ]
            [ Element.row
                [ center, centerY, padding 5, spacing 5, Font.size 5 ]
                [ (iconButton FeatherIcons.arrowUp "Flytta upp" (UpdateQuestion (UpdateChoice questionId (MoveChoiceUp choice.id))))
                , (iconButton FeatherIcons.trash2 "Radera" (UpdateQuestion (UpdateChoice questionId (RemoveChoice choice.id))))
                , (iconButton FeatherIcons.arrowDown "Flytta ned" (UpdateQuestion (UpdateChoice questionId (MoveChoiceDown choice.id))))
                ]
            ]
        ]


questionButtons : QuestionId -> Element Msg
questionButtons questionId =
    Element.column
        [ center, centerY, padding 5, spacing 10 ]
        [ (iconButton FeatherIcons.arrowUp "Flytta upp" (UpdateQuestion (MoveQuestionUp questionId)))
        , (iconButton FeatherIcons.trash2 "Radera" (UpdateQuestion (RemoveQuestion questionId)))
        , (iconButton FeatherIcons.arrowDown "Flytta ned" (UpdateQuestion (MoveQuestionDown questionId)))
        ]


iconButton : FeatherIcons.Icon -> String -> Msg -> Element Msg
iconButton icon titleText msg =
    Element.el
        [ Font.color Color.lightCharcoal
        , Font.mouseOverColor Color.charcoal
        , onClick msg
        , pointer
        ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )
