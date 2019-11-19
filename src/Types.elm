module Types exposing (..)

import Dict exposing (Dict)


type alias MoveLocation =
    ( Int, Int )


type Side
    = Red
    | Blue


type Direction
    = Up
    | Down
    | Left
    | Right


type Border
    = Border Side Direction
    | NoBorder
      
type alias BoardState =
    { tiles : Dict ( Int, Int ) Side
    , size : Int
    }


type alias Move =
    ( ( Int, Int ), Side )
