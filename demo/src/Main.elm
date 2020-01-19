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
            (\count fl str bg color rangeint rangefloat ->
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
                    ]
            )
            |> Bulletproof.Knob.int "count" 10
            |> Bulletproof.Knob.float "float" 10.123
            |> Bulletproof.Knob.string "text" "Counter example!"
            |> Bulletproof.Knob.radio "radio"
                [ ( "Red", Red )
                , ( "Green", Green )
                , ( "Blue", Blue )
                ]
            |> Bulletproof.Knob.select "select"
                [ ( "Black", Black )
                , ( "White", White )
                ]
            |> Bulletproof.Knob.intRange "range int" 10 { min = 0, max = 100, step = 5 }
            |> Bulletproof.Knob.floatRange "range float" 1.5 { min = 0.5, max = 10.5, step = 0.5 }

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
