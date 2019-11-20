module Main exposing (..)

import Browser
import Hexes
import Html exposing (div, input, text)
import Html.Attributes as HA exposing (style, type_, value)
import Html.Events exposing (onInput)
import Noise exposing (noise3d, permutationTable)
import Random exposing (initialSeed)
import Time


type Msg
    = Update (Hexes.GridOptions -> Hexes.GridOptions)
    | Next
    | NoOp


viewControls model =
    div
        [ style "position" "absolute"
        , style "top" "0"
        ]
        [ div []
            [ input
                [ type_ "range"
                , value <| String.fromFloat model.scale
                , HA.min "0.000001"
                , HA.max "0.005"
                , HA.step "0.00001"
                , onInput <|
                    \val ->
                        String.toFloat val
                            |> Maybe.map
                                (\number -> Update (\options -> { options | scale = number }))
                            |> Maybe.withDefault NoOp
                ]
                []
            , text <| "Scale (" ++ (String.fromFloat model.scale) ++ ")"
            ]
              
        , div []
            [ input
                [ type_ "range"
                , value <| String.fromFloat model.strength
                , HA.min "0"
                , HA.max "200"
                , HA.step "1"
                , onInput <|
                    \val ->
                        String.toFloat val
                            |> Maybe.map
                                (\number -> Update (\options -> { options | strength = number }))
                            |> Maybe.withDefault NoOp
                ]
                []
            , text <| "Strength (" ++ (String.fromFloat model.strength) ++ ")"
            ]
        , div []
            [ input
                [ type_ "range"
                , value <| String.fromInt model.cellsAcross
                , HA.min "0"
                , HA.max "50"
                , HA.step "1"
                , onInput <|
                    \val ->
                        String.toInt val
                            |> Maybe.map
                                (\number -> Update (\options -> { options | cellsAcross = number }))
                            |> Maybe.withDefault NoOp
                ]
                []
            , text <| "Cells (" ++ (String.fromInt model.cellsAcross) ++ ")"
            ]
        ]


view model =
    div [ ]
        [ Hexes.permutedHexGrid model
        , viewControls model
        ]


main : Program () Hexes.GridOptions Msg
main =
    let
        ( table, seed ) =
            permutationTable (initialSeed 19)
    in
    Browser.element
        { init = always ( Hexes.defaultOptions table, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Update update_ ->
                        ( update_ model
                        , Cmd.none
                        )

                    Next ->
                        ( { model | zPosition = model.zPosition + 0.01 }, Cmd.none )

                    NoOp ->
                        ( model, Cmd.none )
        , subscriptions = \model -> Time.every 33 <| always Next
        }
