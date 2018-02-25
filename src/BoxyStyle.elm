module BoxyStyle exposing (..)

import Element exposing (..)
import Element.Font as Font
import Color
import Element.Border as Border
import Element.Background as Background


box : List (Attribute msg)
box =
    [ Font.color Color.charcoal
    , Background.color Color.lightOrange
    , mouseOver [ Background.color Color.lightBlue]
    , Border.color Color.charcoal
    , centerX
    , centerY
    , pointer
    , Border.shadow
        { offset = ( 2, 2 )
        , blur = 6
        , size = 3
        , color = Color.darkGray
        }
    ]


h1 : List (Attribute msg)
h1 =
    [ Font.size 20 ]
