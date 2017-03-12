module Helpers exposing (..)

helper : List a -> a -> Int -> Int
helper lst elem offset = 
  case lst of
    []      -> -1
    x :: xs ->
      if x == elem then offset
      else helper xs elem (offset + 1)

indexOf lst element =
  helper lst element 0