module Page.Profile exposing (view, update, Msg)

import Element exposing (..)
import Element.Input as Input exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)
import Data.User as User exposing (..)


-- Msg --


type Msg
    = TextChanged Field String


type Field
    = OrgName



-- Update --


update : Msg -> Maybe User -> Maybe User
update msg maybeUser =
    case maybeUser of
        Nothing ->
            maybeUser

        Just user ->
            let
                userData =
                    getUserData user.userData
            in
                case msg of
                    TextChanged field newText ->
                        case field of
                            OrgName ->
                                let
                                    newUserData =
                                        { userData | orgName = newText }
                                in
                                    Just { user | userData = Just newUserData }


getUserData : Maybe UserData -> UserData
getUserData maybeUserData =
    case maybeUserData of
        Nothing ->
            emptyUserData

        Just maybeUserData ->
            maybeUserData



-- VIEW --


view : Maybe UserData -> Element Styles variation Msg
view userData =
    let
        oldUserData =
            case userData of
                Nothing ->
                    emptyUserData

                Just userData ->
                    userData
    in
        column None
            []
            [ row None [ padding 20 ] [ Element.text "Vi behöver veta lite mer om dig. Var vänlig och fyll i fälten." ]
            , Input.text Field
                [ padding 10 ]
                { onChange = TextChanged OrgName
                , value = oldUserData.orgName
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
                [ center, verticalCenter, width (px 200), height (px 40) ]
                [ (Element.text "Spara") ]
            ]
