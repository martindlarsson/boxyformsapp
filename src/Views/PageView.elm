module Views.PageView exposing (ActivePage(..), bodyId, frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Data.User as User exposing (User)


-- import Data.UserPhoto as UserPhoto exposing (UserPhoto)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util exposing ((=>))
import Views.Spinner exposing (spinner)


{-| Determines which navbar link (if any) will be rendered as active.
Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.
-}
type ActivePage
    = Other
    | Home
    | Login
    | Register
    | MyForms
    | NewForm
    | Profile String



-- | Settings


{-| Take a page's Html and frame it with a header and footer.
The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.
isLoading is for determining whether we should  ding spinner
in the header. (This comes up during slow page transitions.)
-}
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , div [ class "container mt-4" ] [ content ]
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-toggleable-md navbar-light bg-faded" ]
        [ div [ class "navbar-header" ]
            [ a [ class "navbar-brand", Route.href Route.Home ] [ text "BoxyForms" ]
            , lazy2 Util.viewIf isLoading spinner
            , button [ class "navbar-toggler navbar-toggler-right collapsed", (attribute "data-toggle" "collapse"), (attribute "data-target" "#navbarToggler") ] [ span [ class "navbar-toggler-icon" ] [] ]
            ]
        , div [ class "navbar-collapse collapse in", id "navbarToggler" ]
            [ ul [ class "nav navbar-nav navbar-right pull-xs-right" ] <|
                navbarLink (page == Home) Route.Home [ text "Hem" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "Logga in" ]
            ]

        Just user ->
            let
                userImgUrl =
                    case user.imageUrl of
                        Nothing ->
                            ""

                        Just url ->
                            url
            in
                [ --navbarLink (page == Settings) Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
                  navbarLink (page == NewForm) Route.NewForm [ i [ class "ion-compose" ] [], text " Nytt formulär" ]
                , navbarLink (page == MyForms) Route.MyForms [ text "Mina formulär" ]
                , navbarLink
                    (page == Profile user.displayName)
                    (Route.Profile user.displayName)
                    [ img [ class "user-pic", Html.Attributes.src userImgUrl, Html.Attributes.style [ ( "max-width", "30px" ), ( "max-hight", "30px" ) ] ] []
                    , text (String.concat [ " ", user.displayName ])
                    ]
                , navbarLink False Route.Logout [ text "Logga ut" ]
                ]


viewFooter : Html msg
viewFooter =
    footer []
        -- z-index för att ta fram elm-debuggern
        [ div [ class "navbar fixed-bottom navbar-light bg-faded", style [ ( "z-index", "-1" ) ] ]
            [ div [ class "row" ] [ span [] [ text "BoxyForms ..." ] ] ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


{-| This id comes from index.html.
The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.
-}
bodyId : String
bodyId =
    "page-body"
