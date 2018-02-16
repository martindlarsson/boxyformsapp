module Util exposing (..)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Decode
import Array exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.
    session.user
        |> User.Request.foo
        |> Task.attempt Foo
        |> pair { model | something = blah }
-}
pair : a -> b -> ( a, b )
pair first second =
    first => second


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed msg)


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }



-- Array util functions


type InArrayPosition
    = First
    | Middle
    | Last
    | OutOfBounds


getArrayPosition : Int -> Array a -> InArrayPosition
getArrayPosition itemIndex array =
    let
        arrayLength =
            Array.length array
    in
        if (itemIndex == 0) then
            First
        else if (itemIndex == arrayLength - 1) then
            Last
        else if (itemIndex > 0 && itemIndex < arrayLength - 1) then
            Middle
        else
            OutOfBounds


getArrayPositionForInsertion : Int -> Array a -> InArrayPosition
getArrayPositionForInsertion itemIndex array =
    let
        arrayLength =
            Array.length array
    in
        if (itemIndex == 0) then
            First
        else if (itemIndex == arrayLength) then
            Last
        else if (itemIndex > 0 && itemIndex < arrayLength) then
            Middle
        else
            OutOfBounds


type MoveOperation
    = MoveUp
    | MoveDown

-- moveItem : a -> MoveOperation -> Array a -> Array a
-- moveItem itemToMove moveOp oldArray =
-- let
--     arrayLength = Array.length oldArray

--     maybeTuple = Array.toIndexedList oldArray
--                 |> List.filter (\a -> a == itemToMove)

--     newArray = 
-- in
--     case maybeTuple of
--         Nothing -> oldArray

--         Just (index, item) ->
--             if ((index == 0 && moveOp == MoveUp) || (index + 1 == arrayLength && moveOp == MoveDown) || arrayLength == 1)
--                 then oldArray
--             else 

moveItemUp : Int -> Array a -> Array a
moveItemUp itemIndex oldArray =
    let
        arrayPosition =
            getArrayPosition itemIndex oldArray
    in
        case arrayPosition of
            First ->
                oldArray

            OutOfBounds ->
                oldArray

            _ ->
                let
                    maybeItemToMove =
                        Array.get itemIndex oldArray
                in
                    case maybeItemToMove of
                        Nothing ->
                            oldArray

                        Just itemToMove ->
                            let
                                maybeItemToSwitch =
                                    Array.get (itemIndex - 1) oldArray
                            in
                                case maybeItemToSwitch of
                                    Nothing ->
                                        oldArray

                                    Just itemToSwitch ->
                                        let
                                            intermediateArray =
                                                Array.set (itemIndex - 1) itemToMove oldArray

                                            newArray =
                                                Array.set itemIndex itemToSwitch intermediateArray
                                        in
                                            newArray


insertItemIntoArray : a -> Int -> Array a -> Array a
insertItemIntoArray itemToInsert atIndex oldArray =
    let
        inArrayPosition =
            getArrayPositionForInsertion atIndex oldArray
    in
        case inArrayPosition of
            First ->
                Array.append (Array.fromList [ itemToInsert ]) oldArray

            Middle ->
                let
                    firstHalf =
                        Array.slice 0 atIndex oldArray

                    secondHalf =
                        Array.slice atIndex (Array.length oldArray) oldArray

                    newFirstHalf =
                        Array.append firstHalf (fromList [ itemToInsert ])

                    newArray =
                        Array.append newFirstHalf secondHalf
                in
                    newArray

            Last ->
                Array.push itemToInsert oldArray

            OutOfBounds ->
                oldArray


{-| -}
type alias Device =
    { width : Int
    , height : Int
    , phone : Bool
    , tablet : Bool
    , desktop : Bool
    , bigDesktop : Bool
    , portrait : Bool
    }


initialDevice : Device
initialDevice =
    classifyDevice
        { width = 400
        , height = 400
        }


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.
-}
classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice { width, height } =
    { width = width
    , height = height
    , phone = width <= 600
    , tablet = width > 600 && width <= 1200
    , desktop = width > 1200 && width <= 1800
    , bigDesktop = width > 1800
    , portrait = width < height
    }
