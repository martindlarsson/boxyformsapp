module Page.NewForm exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Views.Form as Form exposing (..)


-- Msg --


type Msg
    = SaveForm
    | TextChanged Field String


type Field
    = NoField
    | FormName
    | DateFrom
    | DateTo


type alias Model =
    { x : Int }



-- UPDATE --


update : Msg -> Maybe User -> Cmd msg
update msg user =
    case msg of
        SaveForm ->
            Cmd.none

        TextChanged field value ->
            case field of
                NoField ->
                    Cmd.none

                FormName ->
                    Cmd.none

                DateFrom ->
                    Cmd.none

                DateTo ->
                    Cmd.none



-- VIEW --


view : Maybe User -> Element Styles variation Msg
view user =
    let
        userForm =
            case (validateUser user) of
                UserIsOK ->
                    empty

                NotLoggedIn ->
                    empty

                UserNeedsMoreInfo ->
                    Form.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret."
    in
        column
            None
            [ spacing 20 ]
            [ paragraph H2 [ padding 10 ] [ text "Skapa nytt formulär" ]
            , paragraph None [ padding 10 ] [ text "Här skapar du ditt formulär. Först behöver jag veta namnet på formuläret och när det ska vara tillgångt och till vem." ]
            , userForm
            , Form.textInput "Namn" "Namnet på formuläret" "" (TextChanged FormName) Enabled
            , Form.textInput "Från-datum" "Från detta datum kan användare fylla i formuläret" "" (TextChanged DateFrom) Enabled
            , Form.textInput "Till-datum" "Till och med detta datum kan användare fylla i formuläret" "" (TextChanged DateTo) Enabled
            ]
