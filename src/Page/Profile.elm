module Page.Profile exposing (view, update, Msg)

import Element exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Ports exposing (saveUser)
import Views.Form as Form exposing (..)


-- Msg --


type Msg
    = TextChanged Field String
    | SaveUser


type Field
    = NoField
    | OrgName



-- Update --


update : Msg -> User -> ( User, Cmd msg )
update msg user =
    case msg of
        TextChanged field newText ->
            case field of
                NoField ->
                    ( user, Cmd.none )

                OrgName ->
                    ( { user | orgName = Just newText }, Cmd.none )

        SaveUser ->
            ( user, saveUser (encode user) )



-- VIEW --


view : User -> Element Styles variation Msg
view user =
    let
        orgName =
            case user.orgName of
                Nothing ->
                    ""

                Just name ->
                    name

        isUserValid =
            validateUser (Just user)
    in
        column
            None
            [ spacing 20 ]
            [ when (isUserValid == UserNeedsMoreInfo) (Form.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret.")
            , Form.textInput "Namn" "Inget" user.displayName (TextChanged NoField) Disabled
            , Form.textInput "E-post" "Inget" user.email (TextChanged NoField) Disabled
            , Form.textInput "Visningsnamn" "Namnet på din organisation eller ditt namn" orgName (TextChanged OrgName) Enabled
            , Form.button "Spara" SaveUser
            ]
