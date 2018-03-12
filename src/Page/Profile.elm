module Page.Profile exposing (view, update, Msg(..), Model, init)

import Element exposing (..)
import Element.Keyed as Keyed exposing (..)
import Data.User exposing (..)
import Ports exposing (saveUser)
import Views.Form as Form exposing (..)


-- Msg --


type Msg
    = TextChanged Field String
    | SaveUser
    | UserSaved User


type Field
    = NoField
    | OrgName


type alias Model =
    { user : User
    , persistentUserState : UserState
    }


init : User -> UserState -> Model
init user persUserState =
    Model user persUserState



-- Update --


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        user =
            model.user
    in
        case msg of
            TextChanged field newText ->
                case field of
                    NoField ->
                        ( model, Cmd.none )

                    OrgName ->
                        let
                            newUser =
                                { user | orgName = Just newText }
                        in
                            ( { model | user = newUser }, Cmd.none )

            SaveUser ->
                ( model, saveUser (encode user) )

            UserSaved newUser ->
                ( { model | user = newUser, persistentUserState = validateUser <| Just newUser }, Cmd.none )



-- VIEW --


view : Model -> Element Msg
view model =
    let
        user =
            model.user

        orgName =
            Maybe.withDefault "" user.orgName

        isUserValid =
            model.persistentUserState

        buttonAbility =
            if (String.isEmpty (Maybe.withDefault "" user.orgName)) then
                Disabled
            else
                Enabled

        infoBox =
            if (isUserValid == UserNeedsMoreInfo) then
                [ Keyed.el []
                    ( toString isUserValid
                    , Form.infoBox "Jag vill be dig fylla i detta formulär innan du går vidare och skapar dina egna formulär. Om du inte tillhör en organisation kan du fylla i ditt namn under visningsnamn. Jag använder visningsnamn i dina formulär som författaren av formuläret."
                    )
                ]
            else
                []
    in
        Element.column
            [ alignTop, spacing 20 ]
        <|
            List.append
                infoBox
                [ Form.textInput Singleline (Just "Namn") "Inget" user.displayName (TextChanged NoField) Disabled
                , Form.textInput Singleline (Just "E-post") "Inget" user.email (TextChanged NoField) Disabled
                , Form.textInput Singleline (Just "Visningsnamn") "Namnet på din organisation eller ditt namn" orgName (TextChanged OrgName) Enabled
                , Form.button "Spara" SaveUser [ height (px 40) ] buttonAbility
                ]
