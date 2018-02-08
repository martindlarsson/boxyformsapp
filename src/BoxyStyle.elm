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
    , Background.mouseOverColor Color.lightBlue
    , Border.color Color.charcoal
    , center
    , centerY
    , pointer

    -- , Border.shadow
    --     { offset = ( 5, 5 )
    --     , size = 3
    --     , blur = 20
    --     , color = Color.darkGray
    --     }
    ]


h1 : List (Attribute msg)
h1 =
    [ Font.size 20 ]
