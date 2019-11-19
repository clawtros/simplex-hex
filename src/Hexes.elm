module Hexes exposing (hexPoints, permutedHex, permutedHexGrid, squareTransform, stringFromTuple)

import Noise exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Types exposing (..)


hexPoints : (( Float, Float ) -> ( Float, Float )) -> Float -> String
hexPoints transformPoints h =
    List.range 0 5
        |> List.map
            (\n -> toFloat n * (2 * pi / 6))
        |> List.map
            (\n -> ( sin n, cos n ))
        |> List.map
            (\( x_, y_ ) ->
                let
                    ( x, y ) =
                        transformPoints ( x_ * h / 2, y_ * h / 2 )
                in
                String.fromFloat x
                    ++ ","
                    ++ String.fromFloat y
            )
        |> String.join " "


permutedHex : Float -> PermutationTable -> ( Float, Float ) -> Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
permutedHex zPos table ( posX, posY ) h attrs children =
    let
        scale =
            0.002

        strength =
            h
    in
    polygon
        (attrs
            ++ [ class "hex"
               , points <|
                    hexPoints
                        (\( x_, y_ ) ->
                            let
                                x =
                                    (x_ + posX) * scale

                                y =
                                    (y_ + posY) * scale

                                angle =
                                    noise3d table x y zPos * pi
                            in
                            ( x_ + cos angle * strength
                            , y_ + sin angle * strength
                            )
                        )
                        h
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
                List.range 1 cellsAcross
                    |> List.map
                        (\x ->
                            g
                                [ transform <|
                                    "translate"
                                        ++ (stringFromTuple <| squareTransform tilesize (x - 1) (y - 1))
                                ]
                            <|
                                [ permutedHex
                                    zPos
                                    table
                                    (squareTransform tilesize (x - 1) (y - 1))
                                    tilesize
                                    []
                                    []
                                ]
                        )
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
