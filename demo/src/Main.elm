module Main exposing (main)

import Browser
import Bulletproof
import Bulletproof.Knob
import Counter
import File
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
            (\show count fl str bg color rangeint rangefloat clr date time files ->
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
                    , [ "Day " ++ String.fromInt date.day
                      , "Month " ++ String.fromInt date.month
                      , "Year " ++ String.fromInt date.year
                      ]
                        |> String.join ", "
                        |> text
                    , br [] []
                    , [ "Hours " ++ String.fromInt time.hours
                      , "Minutes " ++ String.fromInt time.minutes
                      ]
                        |> String.join ", "
                        |> text
                    , br [] []
                    , text ("Files count :" ++ String.fromInt (List.length files))
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
            |> Bulletproof.Knob.time "Time" "12:2"
            |> Bulletproof.Knob.files "File"

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
