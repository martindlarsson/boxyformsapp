module Page.Login exposing (view)

import Html exposing (..)
import Element exposing (..)
import BoxyStyle exposing (..)
import Html.Attributes exposing (class, id)


-- import Data.Session as Session exposing (Session)
-- VIEW --


view : Element Styles variation msg
view =
    el None [] (Element.html (div [ id "firebaseui-auth-container" ] []))
