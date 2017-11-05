module Page.Profile exposing (view, update, Msg)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)
import Ports exposing (saveUser)


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
    column None
        [ padding 20 ]
        [ row None [] [ Element.text "Vi behöver veta lite mer om dig. Var vänlig och fyll i fälten." ]
        , row None [] [ Element.text (String.append "Användare: " user.displayName) ]
        , row None [] [ Element.text (String.append "Epost: " user.email) ]
        , Input.text Field
            []
            { onChange = TextChanged OrgName
            , value =
                case user.orgName of
                    Nothing ->
                        ""

                    Just name ->
                        name
            , label =
                Input.placeholder
                    { label = Input.labelAbove (el None [ verticalCenter ] (Element.text "Organisation"))
                    , text = "Din organisation eller ditt namn"
                    }
            , options =
                [ Input.errorBelow (el Error [] (Element.text "Detta fält måste fyllas i"))
                ]
            }
        , row Box
            [ center, verticalCenter, width (px 200), height (px 40), onClick (SaveUser user) ]
            [ (Element.text "Spara") ]
        ]
