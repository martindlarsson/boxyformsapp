module Page.Profile exposing (view, update, Msg)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Ports exposing (saveUser)
import Views.Form as Form exposing (..)


-- Msg --


type Msg
    = TextChanged Field String
    | SaveUser User


type Field
    = OrgName



-- Update --


update : Msg -> User -> ( User, Cmd msg )
update msg user =
    case msg of
        TextChanged field newText ->
            case field of
                OrgName ->
                    ( { user | orgName = Just newText }, Cmd.none )

        SaveUser user ->
            ( user, saveUser (encode user) )



-- VIEW --


view : User -> Element Styles variation Msg
view user =
    let
        textValue =
            case user.orgName of
                Nothing ->
                    ""

                Just name ->
                    name
    in
        wrappedColumn None
            [ spacing 20 ]
            [ --row InfoBox [ padding 10 ] [ Element.text "Jag behöver veta lite mer om dig. Var vänlig och fyll i fälten." ]
              infoBox "Jag behöver veta lite mer om dig. Var vänlig och fyll i fälten."
            , wrappedRow None
                [ padding 20 ]
                [ Element.text "Detta vet jag redan om dig. Ditt namn är "
                , Element.text user.displayName
                , Element.text " och din epost är "
                , Element.text user.email
                , Element.text ". Jag skulle även behöva veta namnet på din organisation, eller om du representerar endast dig själv skriver du in ditt namn i fältet nedan."
                ]
            , Form.textInput "Organisation" "Namnet på din organisation eller ditt namn" textValue (TextChanged OrgName)
            , Form.button "Spara" (SaveUser user)
            ]
