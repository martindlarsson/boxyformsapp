module BoxyStyle exposing (..)

import Element exposing (Device, classifyDevice)
import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Style.Shadow as Shadow


type Styles
    = None
    | Main
    | Page
    | Logo
    | Navigation
    | NavOption
    | ActiveNavOption
    | Box
    | Container
    | H1
    | H2
    | Label
    | Field
    | DisabledField
    | Error
    | InfoBox
    | InfoBoxTop
    | InfoBoxBody
    | Hairline
    | QuestionsView
    | AddQuestionsView
    | AddQuestionButton


sansSerif : List Font
sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


initialDevice : Device
initialDevice =
    classifyDevice
        { width = 400
        , height = 400
        }


boxyStylesheet : Maybe Device -> StyleSheet Styles variation
boxyStylesheet maybeDevice =
    let
        device =
            case maybeDevice of
                Nothing ->
                    initialDevice

                Just device ->
                    device
    in
        Style.styleSheet
            [ style None [] -- It's handy to have a blank style
            , style Main
                [ Color.text Color.darkCharcoal
                , Color.background Color.lightGray
                , Font.typeface sansSerif
                , Font.size 16
                , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
                ]
            , style Navigation
                [ Color.background Color.lightOrange
                , Shadow.box
                    { offset = ( 0, 0 )
                    , size = 3
                    , blur = 4
                    , color = Color.darkGray
                    }
                ]
            , style Page
                [ Color.text Color.darkCharcoal
                ]
            , style H1
                [ Font.size 25
                ]
            , style H2
                [ Font.size 20
                ]
            , style Hairline
                [ Color.background Color.charcoal
                ]
            , style Label
                [ Font.size 25
                , Font.center
                ]
            , style Logo
                [ Font.size 25
                , Font.typeface sansSerif
                ]
            , style NavOption
                [ Font.size 16
                , Font.typeface sansSerif
                ]
            , style ActiveNavOption
                [ Font.size 16
                , Font.typeface sansSerif
                , Font.underline
                , Font.bold
                ]
            , style Field
                [ Border.all 1
                , Border.solid
                , Color.border Color.lightCharcoal
                ]
            , style DisabledField
                [ Border.all 1
                , Border.solid
                , Color.border Color.lightCharcoal
                , Color.background Color.grey
                ]
            , style InfoBoxTop
                [ Color.background Color.lightBlue
                ]
            , style InfoBoxBody
                [ Color.background Color.white
                ]
            , style Box
                [ Transition.all
                , Color.text Color.charcoal
                , Color.background Color.lightOrange
                , Color.border Color.charcoal
                , hover
                    [ Color.text Color.charcoal
                    , Color.background Color.lightBlue
                    , Color.border Color.charcoal
                    , cursor "pointer"
                    ]
                , Shadow.box
                    { offset = ( 5, 5 )
                    , size = 3
                    , blur = 20
                    , color = Color.darkGray
                    }
                ]
            , style Container
                [ Color.text Color.black
                , Color.background Color.lightGrey
                , Color.border Color.lightGrey
                ]
            , style InfoBox
                [ Color.text Color.darkCharcoal
                , Color.background Color.white
                , Color.border Color.darkCharcoal
                , Font.typeface sansSerif
                , Border.dashed
                ]
            , style Error
                [ Color.text Color.red
                ]
            , style QuestionsView
                []
            , style AddQuestionsView
                [ Font.size 40
                , Color.text Color.darkGray

                -- , hover [ Color.text Color.lightOrange ]
                ]
            , style AddQuestionButton
                [ hover [ Color.text Color.lightOrange ]
                ]
            ]
