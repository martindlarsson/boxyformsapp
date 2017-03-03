module Update exposing(..)

import Messages exposing(Msg(..))
import Models exposing(Model, Route(..))
import Routing exposing(parseLocation)
import Material
import Ports exposing(getForm)
import List exposing(head)

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEventsMsg eventsList ->
            -- let
            --     _ = Debug.log "GotEventsMsg" eventsList
            -- in
                ( { model | events = eventsList }, Cmd.none )

        EventFormClicked eventFormId ->
            let
                _ = Debug.log "EventFormClicked" eventFormId
            in
                ( model, getForm eventFormId )

        GotFormMsg formResult ->
            case formResult of
                Ok formList ->
                    let
                        form = head formList
                    in
                        ( { model | form = form }, Cmd.none )
                Err errorMsg ->
                    ( model , Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
                
                _ = Debug.log "OnLocationChange" newRoute
            in
                case newRoute of
                    FormRoute formId ->
                        ( { model | route = newRoute }, getForm formId )
                        
                    _ ->
                        ( { model | route = newRoute }, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Mdl subMsg ->
            Material.update Mdl subMsg model

        NoOp ->
            ( model, Cmd.none )