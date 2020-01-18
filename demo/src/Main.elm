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
            (\count fl str bg color ->
                div
                    [ style "background" (colorToHex bg)
                    , style "color" (colorToHex color)
                    ]
                    [ text str
                    , br [] []
                    , text (String.fromFloat fl)
                    , Counter.view count
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

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
