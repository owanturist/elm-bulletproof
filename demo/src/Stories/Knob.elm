module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Css
import Date
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


storyColor : Bulletproof.Story
storyColor =
    Bulletproof.batch
        [ Bulletproof.story "Color by string"
            (\title hex ->
                Knob.view
                    viewport
                    [ ( title, Knob.Color hex )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Color"
            |> Bulletproof.Knob.string "Value (hex)" "#1ea5fd"

        --
        , Bulletproof.story "Color by color"
            (\title color ->
                Knob.view
                    viewport
                    [ ( title, Knob.Color color.hex )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Color"
            |> Bulletproof.Knob.color "Value" "#1ea5fd"
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyDate : Bulletproof.Story
storyDate =
    Bulletproof.batch
        [ Bulletproof.story "Date by string"
            (\title date ->
                Knob.view
                    viewport
                    [ ( title, Knob.Date date )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Date"
            |> Bulletproof.Knob.string "Value (yyyy-mm-dd)" "2021-02-20"

        --
        , Bulletproof.story "Date by date"
            (\title date ->
                Knob.view
                    viewport
                    [ ( title, Knob.Date (Date.dateToString date) )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Date"
            |> Bulletproof.Knob.date "Value" "1993-02-09"
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyTime : Bulletproof.Story
storyTime =
    Bulletproof.batch
        [ Bulletproof.story "Time by string"
            (\title time ->
                Knob.view
                    viewport
                    [ ( title, Knob.Time time )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Time"
            |> Bulletproof.Knob.string "Value" "16:38"

        --
        , Bulletproof.story "Time by time"
            (\title time ->
                Knob.view
                    viewport
                    [ ( title, Knob.Time (Date.timeToString time) )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Time"
            |> Bulletproof.Knob.time "Value" "06:38"
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyFiles : Bulletproof.Story
storyFiles =
    Bulletproof.story "Files"
        (\title ->
            Knob.view
                viewport
                [ ( title, Knob.Files )
                ]
                Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Files"
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyViewport : Bulletproof.Story
storyViewport =
    Bulletproof.batch
        [ Bulletproof.story "Viewport by input"
            (\title width height ->
                Knob.view
                    (Bulletproof.Knob.Viewport width height)
                    [ ( title, Knob.StoryViewport )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Viewport"
            |> Bulletproof.Knob.int "Width"
                viewport.width
                [ Bulletproof.Knob.min 0
                ]
            |> Bulletproof.Knob.int "Height"
                viewport.height
                [ Bulletproof.Knob.min 0
                ]

        --
        , Bulletproof.story "Viewport by viewport"
            (\title customViewport ->
                Knob.view
                    customViewport
                    [ ( title, Knob.StoryViewport )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Title" "Viewport"
            |> Bulletproof.Knob.viewport
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

        --
        , Bulletproof.label "COLOR"
        , storyColor

        --
        , Bulletproof.label "DATE"
        , storyDate

        --
        , Bulletproof.label "TIME"
        , storyTime

        --
        , Bulletproof.label "FILES"
        , storyFiles

        --
        , Bulletproof.label "VIEWPORT"
        , storyViewport
        ]
