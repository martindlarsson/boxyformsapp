module Form.FormView exposing(..)

import Html exposing (Html, div, text)
import Models exposing(Model)
import Form.Models exposing(..)
import Messages exposing (Msg(..))

import Material
-- import Material.Color as Color
-- import Material.Card as Card
-- import Material.Options as Options exposing (css)
-- import Material.Elevation as Elevation
-- import Material.Button as Button --exposing (..)
-- import Routing exposing(formPath) 

formView : Model -> Html Msg
formView model =
    case model.form of
        Nothing -> text "Fel! inget formulär hittades"

        Just form ->
            div []
            (List.map (\formStep -> formStepView formStep model.mdl) form.formSteps)


formStepView : FormStep -> Material.Model -> Html Msg
formStepView formStep mdl =
    div []
    [ text formStep.stepTitle ]