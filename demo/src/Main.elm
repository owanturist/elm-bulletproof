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


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#f00"

        Green ->
            "#0f0"

        Blue ->
            "#00f"


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Bulletproof.storyOf "Counter positive"
            (\count fl str color ->
                div
                    [ style "background" (colorToHex color)
                    ]
                    [ text str
                    , br [] []
                    , text (String.fromFloat fl)
                    , Counter.view count
                    ]
            )
            |> Bulletproof.Knob.int "count" 10
            |> Bulletproof.Knob.float "float" 10
            |> Bulletproof.Knob.string "text" "Counter example!"
            |> Bulletproof.Knob.radio "radio"
                [ ( "Red", Red )
                , ( "Green", Green )
                , ( "Blue", Blue )
                ]

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
