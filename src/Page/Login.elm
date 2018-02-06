module Page.Login exposing (view)

import Html exposing (..)
import Element exposing (..)
import Html.Attributes exposing (class, id)


-- VIEW --


view : Element msg
view =
    el [] (Element.html (div [ id "firebaseui-auth-container" ] []))
