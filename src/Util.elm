module Util exposing (..)

import Html exposing (Attribute, Html)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as Decode
import Reorderable exposing (Reorderable)


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



-- General Reorderable

type MoveOperation
    = MoveUp
    | MoveDown

moveItem : Reorderable a -> Int -> MoveOperation -> Reorderable a
moveItem oldList index moveOp =
    case moveOp of
        MoveUp ->
            Reorderable.moveUp index oldList
        MoveDown ->
            Reorderable.moveDown index oldList

addItem : Reorderable a -> a -> Reorderable a
addItem oldList item =
    Reorderable.push item oldList


removeItem : Reorderable a -> Int -> Reorderable a
removeItem oldList index =
    Reorderable.drop index oldList