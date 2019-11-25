module Hexes exposing (GridOptions, defaultOptions, permutedHex, permutedHexGrid, squareTransform, stringFromTuple)

import Noise exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


type alias GridOptions =
    { strength : Float
    , scale : Float
    , cellsAcross : Int
    , zPosition : Float
    , table : PermutationTable
    , speed : Float
    }


defaultOptions : PermutationTable -> GridOptions
defaultOptions table =
    { strength = 50
    , cellsAcross = 15
    , scale = 0.002
    , zPosition = 0
    , speed = 0.01
    , table = table
    }

permutedHex : ( Float, Float ) -> Float -> GridOptions -> Svg msg
permutedHex ( posX, posY ) h {scale, strength, zPosition, table} =
    let
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
                                            noise3d table x y zPosition * pi * 2
                                    in
                                    String.fromFloat (x_ + cos noised * strength)
                                        ++ ","
                                        ++ String.fromFloat (y_ + sin noised * strength)
                               )
                    )
                |> String.join " "

        noise =
            noise3d table ((posX + h / 2) * scale) ((posY + h / 2) * scale) zPosition

        colour =
            "hsla("
                ++ (String.fromFloat <| noise * pi * 57.29)
                ++ ", 70%, 50%, "
                ++ (String.fromFloat <| 0.5)
                ++ ")"
    in
    g []
        [ polygon
            [ class "hex"
            , points pointsString
            , fill colour
            , stroke colour
            ]
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


permutedHexGrid : GridOptions -> Svg msg
permutedHexGrid gridOptions =
    let
        tilesize =
            50

        ( height, width ) =
            squareTransform tilesize gridOptions.cellsAcross gridOptions.cellsAcross
    in
    List.range 1 gridOptions.cellsAcross
        |> List.concatMap
            (\y ->
                List.map
                    (\x ->
                        let
                            transformed =
                                squareTransform tilesize (x - 1) (y - 1)

                            element =
                                permutedHex
                                    transformed
                                    tilesize
                                    gridOptions
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
                        gridOptions.cellsAcross
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
