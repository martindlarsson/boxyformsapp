module Views.Assets exposing (error, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)
We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!
-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES --


error : Image
error =
    Image "https://kuwaitlifestyleblog.files.wordpress.com/2016/07/windows_bug6-100581894-primary-idge.jpg?w=608&h=405"



-- USING IMAGES --


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
