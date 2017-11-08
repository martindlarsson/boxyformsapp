module BoxyStyle exposing (..)

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
    | Label
    | Field
    | Error
    | InfoBox
    | InfoBoxTop
    | InfoBoxBody


sansSerif : List Font
sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


stylesheet : StyleSheet Styles variation
stylesheet =
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
            [ Color.background Color.lightOrange ]
        , style Page
            [ Color.text Color.darkCharcoal
            ]
        , style Label
            [ Font.size 25 -- set font size to 25 px
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
        , style InfoBoxTop
            [ Color.background Color.lightBlue
            ]
        , style InfoBoxBody
            [ Color.background Color.white
            ]
        , style Box
            [ Transition.all
            , Color.text Color.lightGrey
            , Color.background Color.orange
            , Color.border Color.charcoal
            , hover
                [ Color.text Color.white
                , Color.background Color.yellow
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

            -- , Shadow.box
            --     { offset = ( 2, 2 )
            --     , size = 2
            --     , blur = 3
            --     , color = Color.gray
            --     }
            ]
        , style Error
            [ Color.text Color.red
            ]
        ]
