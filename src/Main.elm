module Main exposing (..)

import Browser
import Hexes
import Html exposing (div, input, text)
import Html.Attributes as HA exposing (style, type_)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import Noise exposing (noise3d, permutationTable)
import Random exposing (initialSeed)
import Time


type Msg
    = Update Hexes.GridOptions
    | Next
    | NoOp


type alias Range =
    { min : Float
    , max : Float
    , step : Float
    }


viewRangeInput : { name : String, range : Range, decoder : D.Decoder a, update : a -> Hexes.GridOptions, value : String } -> Html.Html Msg
viewRangeInput { name, range, decoder, update, value } =
    div []
        [ input
            [ type_ "range"
            , HA.value value
            , HA.min <| String.fromFloat range.min
            , HA.max <| String.fromFloat range.max
            , HA.step <| String.fromFloat range.step
            , onInput <|
                \val ->
                    D.decodeString decoder val
                        |> Result.map
                            (\number -> Update <| update number)
                        |> Result.withDefault NoOp
            ]
            []
        , text <| name ++ " (" ++ value ++ ")"
        ]


viewControls : Hexes.GridOptions -> Html.Html Msg
viewControls model =
    div
        [ style "position" "absolute"
        , style "top" "0"
        ]
        [ viewRangeInput
            { range =
                { min = 0.000001
                , max = 0.009
                , step = 0.00001
                }
            , name = "Scale"
            , value = String.fromFloat <| model.scale
            , update = \newVal -> { model | scale = newVal }
            , decoder = D.float
            }
        , viewRangeInput
            { range =
                { min = 1
                , max = 200
                , step = 1
                }
            , name = "Strength"
            , value = String.fromFloat <| model.strength
            , update = \newVal -> { model | strength = newVal }
            , decoder = D.float
            }
        , viewRangeInput
            { range =
                { min = 1
                , max = 50
                , step = 1
                }
            , name = "Cells"
            , value = String.fromInt <| model.cellsAcross
            , update = \newVal -> { model | cellsAcross = newVal }
            , decoder = D.int
            }
        , viewRangeInput
            { range =
                { min = 0
                , max = 0.05
                , step = 0.001
                }
            , name = "Speed"
            , value = String.fromFloat <| model.speed
            , update = \newVal -> { model | speed = newVal }
            , decoder = D.float
            }
        ]


view model =
    div []
        [ Hexes.permutedHexGrid model
        , viewControls model
        ]


main : Program () Hexes.GridOptions Msg
main =
    let
        ( table, _ ) =
            permutationTable (initialSeed 19)
    in
    Browser.element
        { init = always ( Hexes.defaultOptions table, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Update newOpts ->
                        ( newOpts
                        , Cmd.none
                        )

                    Next ->
                        ( { model | zPosition = model.zPosition + model.speed }, Cmd.none )

                    NoOp ->
                        ( model, Cmd.none )
        , subscriptions = \model -> Time.every 33 <| always Next
        }
