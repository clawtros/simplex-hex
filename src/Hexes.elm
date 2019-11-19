module Hexes exposing (hexPoints, permutedHex, permutedHexGrid, squareTransform, stringFromTuple)

import Dict exposing (Dict)
import Noise exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Types exposing (..)


type alias Cache =
    Dict ( Float, Float ) Float


hexPoints : (( Float, Float ) -> ( Float, Float )) -> Float -> String
hexPoints transformPoints h =
    ""



-- List.range 0 5
--     |> List.map
--         (\n ->
--             let
--                 angle =
--                     toFloat n * (2 * pi / 6)
--             in
--             transformPoints ( sin angle * h / 2, cos angle * h / 2 )
--                 |> (\( x, y ) ->
--                         String.fromFloat x
--                             ++ ","
--                             ++ String.fromFloat y
--                    )
--         )
--     |> String.join " "


permutedHex : Cache -> Float -> PermutationTable -> ( Float, Float ) -> Float -> List (Svg.Attribute msg) -> List (Svg msg) -> ( Svg msg, Cache )
permutedHex cache zPos table ( posX, posY ) h attrs children =
    let
        scale =
            0.0015

        strength =
            h

        ( pointsString, newCache ) =
            List.range 0 5
                |> List.foldl
                    (\n ( acc, cacheAcc ) ->
                        let
                            angle =
                                toFloat n * (2 * pi / 6)
                        in
                        ( sin angle * h / 2, cos angle * h / 2 )
                            |> (\( x_, y_ ) ->
                                    let
                                        x =
                                            (x_ + posX) * scale

                                        y =
                                            (y_ + posY) * scale

                                        ( noised, store ) =
                                            case Dict.get ( x, y ) cacheAcc of
                                                Just v ->
                                                    ( v, cacheAcc )

                                                Nothing ->
                                                    let
                                                        v =
                                                            noise3d table x y zPos * pi
                                                    in
                                                    ( v, Dict.insert ( x, y ) v cacheAcc )
                                    in
                                    ( acc
                                        ++ " "
                                        ++ String.fromFloat (x_ + cos noised * strength)
                                        ++ ","
                                        ++ String.fromFloat (y_ + sin noised * strength)
                                    , store
                                    )
                               )
                    )
                    ( "", cache )
    in
    ( polygon
        (attrs
            ++ [ class "hex"
               , points pointsString
               , fill <|
                    let
                        noise =
                            noise3d table ((posX + h / 2) * scale) ((posY + h / 2) * scale) zPos

                        shade =
                            128 |> String.fromInt
                    in
                    "hsla("
                        ++ (String.fromFloat <| noise * pi * 57.29)
                        ++ ", 70%, 50%, "
                        ++ (String.fromFloat <| 0.5)
                        ++ ")"
               , stroke "#222"
               ]
        )
        children
    , newCache
    )


squareTransform : Float -> Int -> Int -> ( Float, Float )
squareTransform h x y =
    let
        xmod =
            h * sqrt 3 / 2

        xOffset =
            (toFloat x
                * xmod
            )
                + (if remainderBy 2 y == 0 then
                    xmod / 2

                   else
                    0
                  )

        ymod =
            h * 0.75

        yOffset =
            toFloat y * ymod + h
    in
    ( xOffset, yOffset )


stringFromTuple : ( Float, Float ) -> String
stringFromTuple ( a, b ) =
    "(" ++ String.fromFloat a ++ "," ++ String.fromFloat b ++ ")"


permutedHexGrid : Float -> PermutationTable -> Int -> Svg msg
permutedHexGrid zPos table cellsAcross_ =
    let
        cellsAcross =
            28

        tilesize =
            50

        ( height, width ) =
            squareTransform tilesize cellsAcross cellsAcross
    in
    List.range 1 cellsAcross
        |> List.concatMap
            (\y ->
                List.foldl
                    (\x ( acc, cache ) ->
                        let
                            transformed =
                                squareTransform tilesize (x - 1) (y - 1)

                            ( element, newCache ) =
                                permutedHex
                                    cache
                                    zPos
                                    table
                                    transformed
                                    tilesize
                                    []
                                    []
                        in
                        ( acc
                            ++ [ g
                                    [ transform <|
                                        "translate"
                                            ++ stringFromTuple transformed
                                    ]
                                 <|
                                    [ element
                                    ]
                               ]
                        , newCache
                        )
                    )
                    ( [], Dict.empty )
                    (List.range
                        1
                        cellsAcross
                    )
                    |> Tuple.first
            )
        |> svg
            [ Svg.Attributes.class "grid-container"
            , version "1.1"
            , Svg.Attributes.style "height: 100vmin; width: 100vmin; margin: auto;"
            , viewBox
                ("0 0 "
                    ++ String.fromFloat height
                    ++ " "
                    ++ (String.fromFloat <| width + (tilesize / 2))
                )
            ]
