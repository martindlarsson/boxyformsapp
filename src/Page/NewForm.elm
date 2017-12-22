module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as FormView exposing (..)
import Data.Form exposing (..)


-- import FontAwesome.Web as Icon

import FeatherIcons


-- import Html exposing (..)
-- import Html.Attributes exposing (title)
-- Msg --


type Msg
    = SaveForm
    | TextChanged Field String
    | CheckboxChanged Field Bool
    | AddQuestion QuestionType


type Field
    = NoField
    | FormName
    | Description
    | DateFrom
    | DateTo
    | Public


type QuestionType
    = Info
    | Text
    | Choice
    | YesNo


type alias Model =
    { form : Form
    , userState : UserState
    }


init : User -> Model
init user =
    { userState = validateUser (Just user)
    , form = emptyForm user
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
            -- ++ [ hairline Hairline ]
            ++ (questionsView model)
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
        []
        [ addQuestionView ]
    ]


addQuestionView : Element Styles variation Msg
addQuestionView =
    row AddQuestionsView
        [ center, spacing 20, padding 5, verticalCenter ]
        [ addQuestionButton FeatherIcons.plusCircle "Lägg till..." (AddQuestion Info)
        , addQuestionButton FeatherIcons.alignJustify "Text" (AddQuestion Text)
        , addQuestionButton FeatherIcons.chevronDown "Lista med alternativ" (AddQuestion Choice)
        , addQuestionButton FeatherIcons.checkCircle "Ja/Nej" (AddQuestion YesNo)
        , addQuestionButton FeatherIcons.info "Information" (AddQuestion Info)
        ]


addQuestionButton : FeatherIcons.Icon -> String -> Msg -> Element Styles variation Msg
addQuestionButton icon titleText msg =
    Element.el AddQuestionButton
        [ onClick msg ]
        (Element.html
            (icon |> FeatherIcons.toHtml [])
        )
