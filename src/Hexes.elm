module Hexes exposing (hexPoints, permutedHex, permutedHexGrid, squareTransform, stringFromTuple)

import Dict exposing (Dict)
import Noise exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Types exposing (..)


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


permutedHex : Float -> PermutationTable -> ( Float, Float ) -> Float -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
permutedHex zPos table ( posX, posY ) h attrs children =
    let
        scale =
            0.0015

        strength =
            h

        pointsString =
            List.range 0 5
                |> List.map
                    (\n ->
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

                                        noised =
                                            noise3d table x y zPos * pi * 2
                                    in
                                    String.fromFloat (x_ + cos noised * strength)
                                        ++ ","
                                        ++ String.fromFloat (y_ + sin noised * strength)
                               )
                    )
                |> String.join " "

        noise =
            noise3d table ((posX + h / 2) * scale) ((posY + h / 2) * scale) zPos

        x0 =
            cos (noise * pi) * strength

        y0 =
            sin (noise * pi) * strength
    in
    g []
        [ polygon
            (attrs
                ++ [ class "hex"
                   , points pointsString
                   , fill <|
                        "hsla("
                            ++ (String.fromFloat <| noise * pi * 57.29)
                            ++ ", 70%, 50%, "
                            ++ (String.fromFloat <| 0.5)
                            ++ ")"
                   ]
            )
            []
        ]


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
                List.map
                    (\x ->
                        let
                            transformed =
                                squareTransform tilesize (x - 1) (y - 1)

                            element =
                                permutedHex
                                    zPos
                                    table
                                    transformed
                                    tilesize
                                    []
                                    []
                        in
                        g
                            [ transform <|
                                "translate"
                                    ++ stringFromTuple transformed
                            ]
                        <|
                            [ element
                            ]
                    )
                    (List.range
                        1
                        cellsAcross
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
