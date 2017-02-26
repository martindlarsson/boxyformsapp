module Update exposing(..)

import Messages exposing(..)
import Models exposing(Model)
import Material

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEventsMsg eventsList ->
            let
                _ = Debug.log "GotEventsMsg" eventsList
            in
                ( { model | events = eventsList }, Cmd.none )

        EventClicked ->
            ( model, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Mdl subMsg ->
            Material.update Mdl subMsg model

        NoOp ->
            ( model, Cmd.none )