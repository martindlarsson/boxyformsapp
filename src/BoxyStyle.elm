module BoxyStyle exposing (..)

import Element exposing (..)
import Element.Font as Font
import Color
import Element.Border as Border
import Element.Background as Background


box : Bool -> List (Attribute msg)
box enabled =
    let
        enabledAttributes =
            if enabled then
                [ Background.color Color.lightOrange
                , Font.color Color.charcoal
                , mouseOver [ Background.color Color.lightYellow ]
                , pointer
                ]
            else
                [ Background.color Color.lightBlue
                , Font.color Color.lightGray
                ]
    in
        List.append
            enabledAttributes
            [ Border.color Color.charcoal
            , centerX
            , centerY
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


h2 : List (Attribute msg)
h2 =
    [ Font.size 16 ]
