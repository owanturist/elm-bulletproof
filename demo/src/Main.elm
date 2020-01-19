module Main exposing (main)

import Browser
import Bulletproof
import Bulletproof.Knob
import Counter
import Html exposing (br, div, text)
import Html.Attributes exposing (style)


type Color
    = Red
    | Green
    | Blue
    | Black
    | White


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#f00"

        Green ->
            "#0f0"

        Blue ->
            "#00f"

        Black ->
            "#000"

        White ->
            "#fff"


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Bulletproof.storyOf "Counter positive"
            (\show count fl str bg color rangeint rangefloat clr date ->
                div
                    [ style "background" (colorToHex bg)
                    , style "color" (colorToHex color)
                    ]
                    [ text str
                    , br [] []
                    , text (String.fromFloat fl)
                    , Counter.view count
                    , br [] []
                    , text ("rangeint: " ++ String.fromInt rangeint)
                    , text ("rangefloat: " ++ String.fromFloat rangefloat)
                    , if show then
                        div
                            [ style "width" "100px"
                            , style "height" "100px"
                            , style "background" clr.hex
                            ]
                            []

                      else
                        text ""
                    , text
                        (String.join ", "
                            [ "Day " ++ String.fromInt date.day
                            , "Year " ++ String.fromInt date.year
                            ]
                        )
                    ]
            )
            |> Bulletproof.Knob.bool "Bool" True
            |> Bulletproof.Knob.int "Int" 10
            |> Bulletproof.Knob.float "Float" 10.123
            |> Bulletproof.Knob.string "String" "Counter example!"
            |> Bulletproof.Knob.radio "Radio"
                [ ( "Red", Red )
                , ( "Green", Green )
                , ( "Blue", Blue )
                ]
            |> Bulletproof.Knob.select "Select"
                [ ( "Black", Black )
                , ( "White", White )
                ]
            |> Bulletproof.Knob.intRange "range int" 10 { min = 0, max = 100, step = 5 }
            |> Bulletproof.Knob.floatRange "range float" 1.5 { min = 0.5, max = 10.5, step = 0.5 }
            |> Bulletproof.Knob.color "Color" "#cce"
            |> Bulletproof.Knob.date "Date" "10-2-2011"

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
