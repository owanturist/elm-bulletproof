module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Date
import Dict
import Html.Styled exposing (div)
import Html.Styled.Attributes exposing (style)
import Knob
import Utils exposing (px)


viewport : Bulletproof.Knob.Viewport
viewport =
    Bulletproof.Knob.Viewport 1024 768


replaceIndex : String -> Int -> String
replaceIndex pattern index =
    String.replace "${index}" (String.fromInt index) pattern


generalStory : Bulletproof.Story
generalStory =
    [ Bulletproof.story "Empty"
        (Knob.view viewport Knob.initial [])

    --
    , Bulletproof.story "Multiple"
        (\n ->
            List.range 1 n
                |> List.reverse
                |> List.map (\i -> ( "Knob #" ++ String.fromInt i, Knob.Bool True ))
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.int "Amount of Knobs"
            10
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 1
            , Bulletproof.Knob.max 50
            ]
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


boolStory : Bulletproof.Story
boolStory =
    [ Bulletproof.story "Bool false"
        (\title value ->
            ( title, Knob.Bool value )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Bool"
        |> Bulletproof.Knob.bool "Value" False

    --
    , Bulletproof.story "Bool true"
        (\title value ->
            ( title, Knob.Bool value )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Bool"
        |> Bulletproof.Knob.bool "Value" True
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


stringStory : Bulletproof.Story
stringStory =
    [ Bulletproof.story "String empty"
        (\title value ->
            ( title, Knob.String value )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "String"
        |> Bulletproof.Knob.string "Value" ""

    --
    , Bulletproof.story "String single line"
        (\title value ->
            ( title, Knob.String value )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "String"
        |> Bulletproof.Knob.string "Value" "A single line"

    --
    , Bulletproof.story "Single long line"
        (\title value n ->
            ( title, Knob.String (String.repeat n value) )
                |> List.singleton
                |> Knob.view viewport Knob.initial
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
            ( title, Knob.String value )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "String"
        |> Bulletproof.Knob.string "Value" "First line\nSecond line"
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


intStory : Bulletproof.Story
intStory =
    [ Bulletproof.story "Int input"
        (\title value ->
            ( title
            , Knob.Int False
                value
                { min = Nothing
                , max = Nothing
                , step = Nothing
                }
            )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Int"
        |> Bulletproof.Knob.int "Value [-1000; 1000] step 10"
            0
            [ Bulletproof.Knob.min -1000
            , Bulletproof.Knob.max 1000
            , Bulletproof.Knob.step 10
            ]

    --
    , Bulletproof.story "Int range"
        (\title value min max ->
            ( title
            , Knob.Int True
                value
                { min = min
                , max = max
                , step = Nothing
                }
            )
                |> List.singleton
                |> Knob.view viewport Knob.initial
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
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


floatStory : Bulletproof.Story
floatStory =
    [ Bulletproof.story "Float input"
        (\title value ->
            ( title
            , Knob.Float False
                value
                { min = Nothing
                , max = Nothing
                , step = Nothing
                }
            )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Float"
        |> Bulletproof.Knob.float "Value [-100; 100] step 0.1"
            0
            [ Bulletproof.Knob.min -100
            , Bulletproof.Knob.max 100
            , Bulletproof.Knob.step 0.1
            ]

    --
    , Bulletproof.story "Float range"
        (\title value min max ->
            ( title
            , Knob.Float True
                value
                { min = min
                , max = max
                , step = Nothing
                }
            )
                |> List.singleton
                |> Knob.view viewport Knob.initial
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
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


radioStory : Bulletproof.Story
radioStory =
    [ Bulletproof.story "Radio with options"
        (\title optionNamePattern selectedRadioIndex n ->
            let
                knobState =
                    Dict.insert title
                        (Knob.StringValue (replaceIndex optionNamePattern selectedRadioIndex))
                        Knob.initial
            in
            ( title
            , List.range 1 n
                |> List.map (replaceIndex optionNamePattern)
                |> Knob.Radio
            )
                |> List.singleton
                |> Knob.view viewport knobState
        )
        |> Bulletproof.Knob.string "Title" "Radio"
        |> Bulletproof.Knob.string "Radio name pattern" "Radio #${index}"
        |> Bulletproof.Knob.radio "Selected radio by index"
            [ ( "1", 1 )
            , ( "2", 2 )
            , ( "3", 3 )
            , ( "4", 4 )
            , ( "5", 5 )
            ]
        |> Bulletproof.Knob.int "Amount of Knobs"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 5
            ]

    --
    , Bulletproof.story "Radio with long options"
        (\title longString n width ->
            div
                [ style "width" (px width)
                , style "background" "#ccc"
                ]
                [ ( title
                  , Knob.Radio
                        [ "Option #1"
                        , String.repeat n longString
                        , "Option #3"
                        ]
                  )
                    |> List.singleton
                    |> Knob.view viewport Knob.initial
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
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


selectStory : Bulletproof.Story
selectStory =
    [ Bulletproof.story "Select with options"
        (\title optionNamePattern selectedRadioIndex n ->
            let
                knobState =
                    Dict.insert title
                        (Knob.StringValue (replaceIndex optionNamePattern selectedRadioIndex))
                        Knob.initial
            in
            ( title
            , List.range 1 n
                |> List.map (replaceIndex optionNamePattern)
                |> Knob.Select
            )
                |> List.singleton
                |> Knob.view viewport knobState
        )
        |> Bulletproof.Knob.string "Title" "Select"
        |> Bulletproof.Knob.string "Option name pattern" "Option #${index}"
        |> Bulletproof.Knob.select "Selected option by index"
            [ ( "1", 1 )
            , ( "2", 2 )
            , ( "3", 3 )
            , ( "4", 4 )
            , ( "5", 5 )
            ]
        |> Bulletproof.Knob.int "Amount of Knobs"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 5
            ]

    --
    , Bulletproof.story "Select with long options"
        (\title longString n width ->
            div
                [ style "width" (px width)
                , style "background" "#ccc"
                ]
                [ ( title
                  , Knob.Select
                        [ "Option #1"
                        , String.repeat n longString
                        , "Option #3"
                        ]
                  )
                    |> List.singleton
                    |> Knob.view viewport Knob.initial
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
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


colorStory : Bulletproof.Story
colorStory =
    [ Bulletproof.story "Color by string"
        (\title hex ->
            ( title, Knob.Color hex )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Color"
        |> Bulletproof.Knob.string "Value (hex)" "#1ea5fd"

    --
    , Bulletproof.story "Color by color"
        (\title color ->
            ( title, Knob.Color color.hex )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Color"
        |> Bulletproof.Knob.color "Value" "#1ea5fd"
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


dateStory : Bulletproof.Story
dateStory =
    [ Bulletproof.story "Date by string"
        (\title date ->
            ( title, Knob.Date date )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Date"
        |> Bulletproof.Knob.string "Value (yyyy-mm-dd)" "2021-02-20"

    --
    , Bulletproof.story "Date by date"
        (\title date ->
            ( title, Knob.Date (Date.dateToString date) )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Date"
        |> Bulletproof.Knob.date "Value" "1993-02-09"
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


timeStory : Bulletproof.Story
timeStory =
    [ Bulletproof.story "Time by string"
        (\title time ->
            ( title, Knob.Time time )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Time"
        |> Bulletproof.Knob.string "Value (hh:mm)" "16:38"

    --
    , Bulletproof.story "Time by time"
        (\title time ->
            ( title, Knob.Time (Date.timeToString time) )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Time"
        |> Bulletproof.Knob.time "Value" "06:38"
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


filesStory : Bulletproof.Story
filesStory =
    Bulletproof.story "Files"
        (\title ->
            ( title, Knob.Files )
                |> List.singleton
                |> Knob.view viewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Files"
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


viewportStory : Bulletproof.Story
viewportStory =
    [ Bulletproof.story "Viewport by input"
        (\title width height ->
            ( title, Knob.StoryViewport )
                |> List.singleton
                |> Knob.view (Bulletproof.Knob.Viewport width height) Knob.initial
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
            ( title, Knob.StoryViewport )
                |> List.singleton
                |> Knob.view customViewport Knob.initial
        )
        |> Bulletproof.Knob.string "Title" "Viewport"
        |> Bulletproof.Knob.viewport
    ]
        |> Bulletproof.batch
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


story : Bulletproof.Story
story =
    Bulletproof.folder "Knob"
        [ generalStory

        --
        , Bulletproof.label "BOOL"
        , boolStory

        --
        , Bulletproof.label "STRING"
        , stringStory

        --
        , Bulletproof.label "INT"
        , intStory

        --
        , Bulletproof.label "FLOAT"
        , floatStory

        --
        , Bulletproof.label "RADIO"
        , radioStory

        --
        , Bulletproof.label "SELECT"
        , selectStory

        --
        , Bulletproof.label "COLOR"
        , colorStory

        --
        , Bulletproof.label "DATE"
        , dateStory

        --
        , Bulletproof.label "TIME"
        , timeStory

        --
        , Bulletproof.label "FILES"
        , filesStory

        --
        , Bulletproof.label "VIEWPORT"
        , viewportStory
        ]
