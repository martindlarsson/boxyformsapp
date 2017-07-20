module Helpers.ListHelpers exposing (..)

import List exposing (..)

helper : List a -> a -> Int -> Int
helper lst elem offset = 
  case lst of
    []      -> -1
    x :: xs ->
      if x == elem then offset
      else helper xs elem (offset + 1)

indexOf lst element =
  helper lst element 0

lastInList : List a -> Maybe a
lastInList list =
  reverse list
    |> head


listTail : List a -> List a
listTail list =
    case (tail list) of
      Nothing -> []

      Just newList -> newList


dropLast : List a -> List a
dropLast list =
  reverse list
  |> drop 1
  |> reverse