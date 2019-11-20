module Main exposing (..)

import Browser
import Hexes
import Noise exposing (noise3d, permutationTable)
import Random exposing (initialSeed)
import Time


type Msg
    = UpdateZ


main : Program () Float Msg
main =
    let
        ( table, seed ) =
            permutationTable (initialSeed 19)
    in
    Browser.element
        { init = always ( 0.01, Cmd.none )
        , view = \model -> Hexes.permutedHexGrid model table 25 0.001
        , update =
            \msg model ->
                case msg of
                    UpdateZ ->
                        ( model + 0.005, Cmd.none )
        , subscriptions = always <| Time.every 33 <| always UpdateZ
        }
