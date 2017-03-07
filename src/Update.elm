module Update exposing(..)

import Messages exposing(Msg(..))
import Models exposing(Model, Route(..))
import Routing exposing(parseLocation)
import Material
import Ports exposing(getForm)
import List exposing(head)
import Form.Models exposing(Answer, QuestionId)

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

        SetAnswer questionId answer ->
            let
                -- _ = Debug.log "SetAnswer" ((toString questionId) ++ ", svar: " ++ answer )
                newAnsers = updateAnswers model questionId answer
            in
                ( { model | answers = Just newAnsers }, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Mdl subMsg ->
            Material.update Mdl subMsg model

        NoOp ->
            ( model, Cmd.none )



updateAnswers : Model -> QuestionId -> String -> List Answer
updateAnswers model questionId answer =
    let
        newAnswer = [ Answer questionId answer ]
        oldAnswers =
            case model.answers of
                Nothing -> []
                
                -- if not empty, filter out the same answer
                Just answers -> answers
        keepAnswers =
            List.filter (\answer -> answer.questionId /= questionId) oldAnswers
        _ = Debug.log "updateAnsers" (keepAnswers ++ newAnswer)
    in
        keepAnswers ++ newAnswer