module Demo exposing (main)

import Bulletproof
import Bulletproof.Knob
import Counter
import Html exposing (br, div, text)
import Html.Attributes exposing (style)
import Incrementor
import Story.Checkbox
import Story.Link


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
        [ Story.Link.story
        , Story.Checkbox.story
        , Bulletproof.storyOf "Counter positive"
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
                    |> Bulletproof.html
            )
            |> Bulletproof.Knob.bool "Bool" True
            |> Bulletproof.Knob.int "Int"
                10
                [ Bulletproof.Knob.min 5
                , Bulletproof.Knob.max 50
                , Bulletproof.Knob.step 5
                ]
            |> Bulletproof.Knob.float "Float" 10.123 []
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
            |> Bulletproof.Knob.int "range int"
                10
                [ Bulletproof.Knob.range True
                ]
            |> Bulletproof.Knob.float "range float"
                1.5
                [ Bulletproof.Knob.range True
                , Bulletproof.Knob.step 0.1
                ]
            |> Bulletproof.Knob.color "Color" "#cce"
            |> Bulletproof.Knob.date "Date" "10-2-2011"
            |> Bulletproof.Knob.time "Time" "12:2"
            |> Bulletproof.Knob.files "File"

        --
        , Bulletproof.storyOf "Incrementor initial"
            (Incrementor.view 0
                |> Bulletproof.html
            )
        ]
