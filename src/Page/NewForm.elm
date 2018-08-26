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
import Data.Form as Form exposing (..)
import FeatherIcons
import Util exposing (..)
import Reorderable exposing (Reorderable)
import Ports exposing (saveForm)
import Random.String exposing (string)
import Random.Char exposing (english)
import Random exposing (generate)


-- Msg --


type Msg
    = SaveForm
    | FormSaved
    | UpdateFormId String
    | UpdateFormMeta Field
    | UpdateQuestion QuestionOperation


type MsgForParent
    = NoMsg
    | AddNewForm Form


type QuestionOperation
    = AddQuestion QuestionType ControlIndex
    | UpdateQuestionText QuestionIdx String
    | UpdateQuestionRequired QuestionIdx Bool
    | MoveQuestionUp QuestionIdx
    | MoveQuestionDown QuestionIdx
    | RemoveQuestion QuestionIdx
    | UpdateChoice QuestionIdx ChoiceOperation
    | AddQuestionControlHover ControlIndex
    | AddQuestionControlNoHover


type ChoiceOperation
    = AddChoice
    | UpdateChoiceText ChoiceIdx String
    | MoveChoiceUp ChoiceIdx
    | MoveChoiceDown ChoiceIdx
    | RemoveChoice ChoiceIdx


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


stringGenerator : Random.Generator String
stringGenerator =
    string 10 Random.Char.english


init : User -> Device -> Maybe Form -> ( Model, Cmd Msg )
init user device maybeForm =
    let
        _ =
            Debug.log "initForm" (toString maybeForm)

        form =
            case maybeForm of
                Nothing ->
                    emptyForm user

                Just formToEdit ->
                    formToEdit

        cmd =
            case maybeForm of
                Nothing ->
                    Random.generate UpdateFormId stringGenerator

                _ ->
                    Cmd.none

        -- if (maybeForm === Nothing) then
        --     Cmd.none
        -- else
        --     Random.generate UpdateFormId stringGenerator
        _ =
            Debug.log "initForm cmd" (toString cmd)
    in
        ( { userState = validateUser (Just user)
          , form = form
          , controlHoverState = NoControl
          , device = device
          }
        , cmd
        )


update : Msg -> Model -> ( ( Model, Cmd Msg ), MsgForParent )
update msg model =
    let
        oldForm =
            model.form
    in
        case msg of
            SaveForm ->
                -- TODO, Cmd AddNewForm model.form ska bara köras en gång
                ( ( model, saveForm (encodeForm model.form) ), AddNewForm model.form )

            FormSaved ->
                -- TODO, ge feedback till användaren att formuläret sparades
                let
                    _ =
                        Debug.log "newForm update" "FormSaved"
                in
                    ( ( model, Cmd.none ), NoMsg )

            UpdateFormMeta field ->
                ( ( { model | form = updateFormMetadata oldForm field }, Cmd.none ), NoMsg )

            UpdateQuestion questionOperation ->
                let
                    ( newQuestions, newControlHoverState ) =
                        updateQuestion oldForm.questions questionOperation model.controlHoverState

                    newForm =
                        { oldForm | questions = newQuestions }
                in
                    ( ( { model | form = newForm, controlHoverState = newControlHoverState }, Cmd.none ), NoMsg )

            UpdateFormId newFormId ->
                ( ( { model | form = { oldForm | id = newFormId } }, Cmd.none ), NoMsg )


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


updateQuestion : Reorderable Question -> QuestionOperation -> ControlHover -> ( Reorderable Question, ControlHover )
updateQuestion oldQuestions qOperation oldControlHoverState =
    let
        errorReturn =
            ( oldQuestions, oldControlHoverState )
    in
        case qOperation of
            AddQuestion qType index ->
                ( addQuestion oldQuestions qType index
                , oldControlHoverState
                )

            UpdateQuestionText questionIdx newText ->
                let
                    updateQuestionText oldQuestion =
                        { oldQuestion | questionText = newText }

                    newQuestions =
                        Form.updateQuestion oldQuestions questionIdx updateQuestionText
                in
                    ( newQuestions, oldControlHoverState )

            UpdateQuestionRequired questionIdx newValue ->
                let
                    updateQuestionText oldQuestion =
                        { oldQuestion | required = newValue }

                    newQuestions =
                        Form.updateQuestion oldQuestions questionIdx updateQuestionText
                in
                    ( newQuestions, oldControlHoverState )

            MoveQuestionUp questionIdx ->
                let
                    newQuestions =
                        moveItem oldQuestions questionIdx MoveUp
                in
                    ( newQuestions, oldControlHoverState )

            MoveQuestionDown questionIdx ->
                let
                    newQuestions =
                        moveItem oldQuestions questionIdx MoveDown
                in
                    ( newQuestions, oldControlHoverState )

            RemoveQuestion questionIdx ->
                let
                    newQuestions =
                        removeItem oldQuestions questionIdx
                in
                    ( newQuestions, oldControlHoverState )

            UpdateChoice questionIdx choiceOperation ->
                let
                    maybeOldQuestion =
                        getItem oldQuestions questionIdx
                in
                    case maybeOldQuestion of
                        Nothing ->
                            errorReturn

                        Just oldQuestion ->
                            case oldQuestion.questionType of
                                ChoiceType choiceList ->
                                    let
                                        newChoices =
                                            updateChoice questionIdx choiceList choiceOperation

                                        newQuestion =
                                            { oldQuestion | questionType = ChoiceType newChoices }

                                        replaceOldQuestion oldQuestion =
                                            newQuestion

                                        newQuestions =
                                            Form.updateQuestion oldQuestions questionIdx replaceOldQuestion
                                    in
                                        ( newQuestions, oldControlHoverState )

                                _ ->
                                    errorReturn

            AddQuestionControlHover controlIndex ->
                ( oldQuestions, Hovering controlIndex )

            AddQuestionControlNoHover ->
                ( oldQuestions, NoControl )


updateChoice : QuestionIdx -> Reorderable Choice -> ChoiceOperation -> Reorderable Choice
updateChoice questionIdx oldChoices choiceOp =
    case choiceOp of
        AddChoice ->
            Util.addItem oldChoices emptyChoice

        UpdateChoiceText choiceIdx newText ->
            Form.updateChoice oldChoices choiceIdx newText

        MoveChoiceUp choiceIdx ->
            Util.moveItem oldChoices choiceIdx MoveUp

        MoveChoiceDown choiceIdx ->
            Util.moveItem oldChoices choiceIdx MoveDown

        RemoveChoice choiceIdx ->
            Util.removeItem oldChoices choiceIdx



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
            , FormView.button "Spara" SaveForm [ height (px 40) ] Enabled
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
                    Element.none

                NotLoggedIn ->
                    Element.none

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
        keyedList =
            getIndexedList model.form.questions

        questionViews =
            List.map (\( index, question ) -> (questionTuple model.controlHoverState question index model.device)) keyedList
    in
        Element.map UpdateQuestion <|
            Element.column []
                ([ addQuestionView model.controlHoverState 0 model.device ]
                    ++ questionViews
                )


questionTuple : ControlHover -> Question -> Int -> Device -> Element QuestionOperation
questionTuple hoverState question index device =
    Element.column
        [ spacing 20, padding 10 ]
        [ questionView question index
        , addQuestionView hoverState (index + 1) device
        ]


addQuestionView : ControlHover -> ControlIndex -> Device -> Element QuestionOperation
addQuestionView hoverState index device =
    let
        plus =
            [ addQuestionButton FeatherIcons.plusCircle "Lägg till fråga" (AddQuestion InfoType index) ]

        allControls =
            [ addQuestionButton FeatherIcons.alignJustify "Text" (AddQuestion TextType index)
            , addQuestionButton FeatherIcons.chevronDown "Val" (AddQuestion (ChoiceType Reorderable.empty) index)
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
            , Background.color Color.lightGray
            , centerY
            , centerX
            , width (px 350)
            , height (px 40)
            , onMouseEnter (AddQuestionControlHover index)
            , onMouseLeave AddQuestionControlNoHover
            ]
            controls


addQuestionButton : FeatherIcons.Icon -> String -> QuestionOperation -> Element QuestionOperation
addQuestionButton icon titleText msg =
    Element.column
        [ centerY
        , centerX
        , pointer
        , Font.color Color.darkGray
        , mouseOver [ Font.color Color.lightCharcoal ]
        , onClick msg
        , width (px 80)
        , padding 10
        ]
        [ Element.el
            [ centerY, centerX ]
            --, Element.Attributes.toAttr (Html.Attributes.title titleText)
            (Element.html
                (icon |> FeatherIcons.toHtml [])
            )
        , Element.el [ Font.size 10, centerX, centerY ] (Element.text titleText)
        ]


questionView : Question -> QuestionIdx -> Element QuestionOperation
questionView question questionIndex =
    let
        questionType =
            question.questionType

        ( questionContent, labelText ) =
            case questionType of
                TextType ->
                    ( textQuestion question questionIndex, "Textfråga" )

                InfoType ->
                    ( infoQuestion question questionIndex, "Informationstext" )

                ChoiceType choices ->
                    ( choiceQuestion question questionIndex choices, "Flervalsfråga" )

                YesNoType ->
                    ( yesNoQuestion question questionIndex, "Ja/Nej-fråga" )
    in
        Element.column
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
            [ Element.row [ height (px 25) ]
                [ Element.column [ centerY ] [ Element.el [ alignLeft, centerY, Font.size 18 ] (Element.text labelText) ]
                , if questionType /= InfoType then
                    Element.column [ width (px 100), alignRight, spacing 10 ] [ FormView.checkbox "Obligatorisk" (UpdateQuestionRequired questionIndex) question.required ]
                  else
                    Element.none
                , Element.column [ width (px 100), alignRight, centerY, spacing 5 ] [ questionButtons question questionIndex ]
                ]
            , Element.row [] [ questionContent ]
            ]


infoQuestion : Question -> QuestionIdx -> Element QuestionOperation
infoQuestion question questionIdx =
    FormView.textInput
        Multiline
        Nothing
        "Här kan du skriva en informativ text som hjälper användaren."
        question.questionText
        (UpdateQuestionText questionIdx)
        Enabled


textQuestion : Question -> QuestionIdx -> Element QuestionOperation
textQuestion question questionIdx =
    FormView.textInput
        Multiline
        Nothing
        "Frågetext"
        question.questionText
        (UpdateQuestionText questionIdx)
        Enabled


yesNoQuestion : Question -> QuestionIdx -> Element QuestionOperation
yesNoQuestion question questionIdx =
    FormView.textInput
        Multiline
        Nothing
        "Frågetext"
        question.questionText
        (UpdateQuestionText questionIdx)
        Enabled


choiceQuestion : Question -> QuestionIdx -> Reorderable Choice -> Element QuestionOperation
choiceQuestion question questionIdx choiceList =
    let
        listLength =
            List.length <| Reorderable.toList choiceList

        elementHeight =
            60 + listLength * 60

        indexedList =
            getIndexedList choiceList
    in
        Element.column [ spacing 5, Font.alignLeft ]
            [ FormView.textInput
                Singleline
                Nothing
                "Frågetext"
                question.questionText
                (UpdateQuestionText questionIdx)
                Enabled
            , Element.row [] [ Element.el [ alignLeft, padding 5 ] (Element.text "Val") ]
            , Keyed.row
                [ Background.color Color.white
                , height (px elementHeight)
                , Border.color Color.charcoal
                , Border.width 1
                , Border.solid
                , Border.rounded 3
                , spacing 5
                ]
                [ ( "choices " ++ (toString questionIdx)
                  , (Element.column []
                        (List.append
                            (List.map (\( idx, c ) -> choiceView c idx questionIdx) indexedList)
                            [ addQuestionButton FeatherIcons.plusCircle "Lägg till val" (UpdateChoice questionIdx AddChoice) ]
                        )
                    )
                  )
                ]
            ]


choiceView : Choice -> ChoiceIdx -> QuestionIdx -> Element QuestionOperation
choiceView choice choiceIdx questionIdx =
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
            [ FormView.textInput Singleline Nothing choice.choiceText choice.choiceText (\text -> (UpdateChoice questionIdx (UpdateChoiceText choiceIdx text))) Enabled ]
        , Element.column [ width (px 100), centerY, centerX ]
            [ Element.row
                [ centerX, centerY, padding 5, spacing 5, Font.size 5 ]
                [ (iconButton FeatherIcons.arrowUp "Flytta upp" (UpdateChoice questionIdx (MoveChoiceUp choiceIdx)))
                , (iconButton FeatherIcons.trash2 "Radera" (UpdateChoice questionIdx (RemoveChoice choiceIdx)))
                , (iconButton FeatherIcons.arrowDown "Flytta ned" (UpdateChoice questionIdx (MoveChoiceDown choiceIdx)))
                ]
            ]
        ]


questionButtons : Question -> QuestionIdx -> Element QuestionOperation
questionButtons question questionIdx =
    Element.row
        [ centerX, centerY, padding 5, spacing 10 ]
        [ (iconButton FeatherIcons.arrowUp "Flytta upp" (MoveQuestionUp questionIdx))
        , (iconButton FeatherIcons.trash2 "Radera" (RemoveQuestion questionIdx))
        , (iconButton FeatherIcons.arrowDown "Flytta ned" (MoveQuestionDown questionIdx))
        ]


iconButton : FeatherIcons.Icon -> String -> QuestionOperation -> Element QuestionOperation
iconButton icon titleText msg =
    Element.el
        [ Font.color Color.lightCharcoal
        , mouseOver [ Font.color Color.charcoal ]
        , onClick msg
        , pointer
        , centerY
        , centerX
        ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )
