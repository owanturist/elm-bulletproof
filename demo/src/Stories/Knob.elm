module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Css
import Html.Styled exposing (div, styled)
import Knob


viewport : Bulletproof.Knob.Viewport
viewport =
    Bulletproof.Knob.Viewport 1024 768


storyGeneral : Bulletproof.Story
storyGeneral =
    Bulletproof.batch
        [ Bulletproof.story "Empty"
            (Knob.view viewport [] Knob.initial)

        --
        , Bulletproof.story "Multiple"
            (\n ->
                Knob.view viewport
                    (List.map
                        (\i ->
                            ( "Knob #" ++ String.fromInt i, Knob.Bool True )
                        )
                        (List.reverse (List.range 1 n))
                    )
                    Knob.initial
            )
            |> Bulletproof.Knob.int "Amount of Knobs"
                10
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 50
                ]

        --
        , Bulletproof.story "Color"
            (\hex ->
                Knob.view
                    viewport
                    [ ( "Color", Knob.Color hex )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Color (hex)" "#1ea5fd"

        --
        , Bulletproof.story "Date"
            (\date ->
                Knob.view
                    viewport
                    [ ( "Date", Knob.Date date )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Date" "2021-02-20"

        --
        , Bulletproof.story "Time"
            (\time ->
                Knob.view
                    viewport
                    [ ( "Time", Knob.Time time )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Time" "16:38"

        --
        , Bulletproof.story "Files"
            (Knob.view
                viewport
                [ ( "Files", Knob.Files )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Viewport"
            (\useCustomViewport width height storyViewport ->
                Knob.view
                    (if useCustomViewport then
                        { width = width, height = height }

                     else
                        storyViewport
                    )
                    [ ( "Viewport", Knob.StoryViewport )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.bool "Use custom viewport" False
            |> Bulletproof.Knob.int "Width" viewport.width []
            |> Bulletproof.Knob.int "Height" viewport.height []
            |> Bulletproof.Knob.viewport
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyBool : Bulletproof.Story
storyBool =
    Bulletproof.batch
        [ Bulletproof.story "Bool false"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.Bool value )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Bool"
            |> Bulletproof.Knob.bool "Value" False

        --
        , Bulletproof.story "Bool true"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.Bool value )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Bool"
            |> Bulletproof.Knob.bool "Value" True
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyString : Bulletproof.Story
storyString =
    Bulletproof.batch
        [ Bulletproof.story "String empty"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.String value )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "String"
            |> Bulletproof.Knob.string "Value" ""

        --
        , Bulletproof.story "String single line"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.String value )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "String"
            |> Bulletproof.Knob.string "Value" "A single line"

        --
        , Bulletproof.story "Single long line"
            (\title value n ->
                Knob.view viewport
                    [ ( title, Knob.String (String.repeat n value) )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "String"
            |> Bulletproof.Knob.string "Value" "A_single_long_line"
            |> Bulletproof.Knob.int "Repeat the String N times"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]

        --
        , Bulletproof.story "String multiline"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.String value )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "String"
            |> Bulletproof.Knob.string "Value" "First line\nSecond line"
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyInt : Bulletproof.Story
storyInt =
    Bulletproof.batch
        [ Bulletproof.story "Int input"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.Int False value { min = Nothing, max = Nothing, step = Nothing } )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Int"
            |> Bulletproof.Knob.int "Value" 0 []

        --
        , Bulletproof.story "Int range"
            (\title value min max ->
                Knob.view viewport
                    [ ( title, Knob.Int True value { min = min, max = max, step = Nothing } )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Int"
            |> Bulletproof.Knob.int "Value"
                0
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min -150
                , Bulletproof.Knob.max 150
                , Bulletproof.Knob.step 10
                ]
            |> Bulletproof.Knob.radio "Min"
                [ ( "Nothing", Nothing )
                , ( "Just 0", Just 0 )
                , ( "Just -100", Just -100 )
                , ( "Just 100", Just 100 )
                ]
            |> Bulletproof.Knob.radio "Max"
                [ ( "Nothing", Nothing )
                , ( "Just 0", Just 0 )
                , ( "Just -100", Just -100 )
                , ( "Just 100", Just 100 )
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyFloat : Bulletproof.Story
storyFloat =
    Bulletproof.batch
        [ Bulletproof.story "Float input"
            (\title value ->
                Knob.view viewport
                    [ ( title, Knob.Float False value { min = Nothing, max = Nothing, step = Nothing } )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Float"
            |> Bulletproof.Knob.float "Value" 0 []

        --
        , Bulletproof.story "Float range"
            (\title value min max ->
                Knob.view viewport
                    [ ( title, Knob.Float True value { min = min, max = max, step = Nothing } )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Float"
            |> Bulletproof.Knob.float "Value"
                0
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min -15
                , Bulletproof.Knob.max 15
                , Bulletproof.Knob.step 0.1
                ]
            |> Bulletproof.Knob.radio "Min"
                [ ( "Nothing", Nothing )
                , ( "Just 0", Just 0 )
                , ( "Just -10", Just -10 )
                , ( "Just 10", Just 10 )
                ]
            |> Bulletproof.Knob.radio "Max"
                [ ( "Nothing", Nothing )
                , ( "Just 0", Just 0 )
                , ( "Just -10", Just -10 )
                , ( "Just 10", Just 10 )
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyRadio : Bulletproof.Story
storyRadio =
    Bulletproof.batch
        [ Bulletproof.story "Radio with options"
            (\title optionNamePattern n ->
                Knob.view viewport
                    [ ( title
                      , List.range 1 n
                            |> List.map (\i -> String.replace "${index}" (String.fromInt i) optionNamePattern)
                            |> Knob.Radio
                      )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Radio"
            |> Bulletproof.Knob.string "Option Name pattern" "Option #${index}"
            |> Bulletproof.Knob.radio "Amount of Options"
                [ ( "2", 2 )
                , ( "5", 5 )
                , ( "10", 10 )
                , ( "20", 20 )
                ]

        --
        , Bulletproof.story "Radio with long options"
            (\title longString n width ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view viewport
                        [ ( title
                          , Knob.Radio
                                [ "Option #1"
                                , String.repeat n longString
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
            )
            |> Bulletproof.Knob.string "Title" "Radio"
            |> Bulletproof.Knob.string "Value" "A_single_long_line"
            |> Bulletproof.Knob.int "Repeat the String N times"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]
            |> Bulletproof.Knob.int "Container width in px"
                300
                [ Bulletproof.Knob.min 0
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storySelect : Bulletproof.Story
storySelect =
    Bulletproof.batch
        [ Bulletproof.story "Select with options"
            (\title optionNamePattern n ->
                Knob.view viewport
                    [ ( title
                      , List.range 1 n
                            |> List.map (\i -> String.replace "${index}" (String.fromInt i) optionNamePattern)
                            |> Knob.Select
                      )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Select"
            |> Bulletproof.Knob.string "Option Name pattern" "Option #${index}"
            |> Bulletproof.Knob.select "Amount of Options"
                [ ( "2", 2 )
                , ( "5", 5 )
                , ( "10", 10 )
                , ( "20", 20 )
                ]

        --
        , Bulletproof.story "Select with long options"
            (\title longString n width ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view viewport
                        [ ( title
                          , Knob.Select
                                [ "Option #1"
                                , String.repeat n longString
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
            )
            |> Bulletproof.Knob.string "Title" "Select"
            |> Bulletproof.Knob.string "Value" "A_single_long_line"
            |> Bulletproof.Knob.int "Repeat the String N times"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]
            |> Bulletproof.Knob.int "Container width in px"
                300
                [ Bulletproof.Knob.min 0
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


story : Bulletproof.Story
story =
    Bulletproof.folder "Knob"
        [ storyGeneral

        --
        , Bulletproof.label "BOOL"
        , storyBool

        --
        , Bulletproof.label "STRING"
        , storyString

        --
        , Bulletproof.label "INT"
        , storyInt

        --
        , Bulletproof.label "FLOAT"
        , storyFloat

        --
        , Bulletproof.label "RADIO"
        , storyRadio

        --
        , Bulletproof.label "SELECT"
        , storySelect
        ]
