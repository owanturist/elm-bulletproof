module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Color
import Css
import Date
import Html.Styled exposing (div, styled)
import Knob
import Time


viewport : Bulletproof.Knob.Viewport
viewport =
    Bulletproof.Knob.Viewport 1024 768


storyString : Bulletproof.Story
storyString =
    Bulletproof.folder "String"
        [ Bulletproof.story "Empty"
            (Knob.view viewport
                [ ( "String", Knob.String "" )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Single line"
            (Knob.view viewport
                [ ( "String", Knob.String "A single line" )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Single long line"
            (\n ->
                Knob.view viewport
                    [ ( "String", Knob.String (String.repeat n "ASingleLongLine") )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Repeat String"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]

        --
        , Bulletproof.story "Multiline"
            (\n ->
                Knob.view viewport
                    [ ( "String"
                      , List.range 1 n
                            |> List.map (\i -> "A line #" ++ String.fromInt i)
                            |> String.join "\n"
                            |> Knob.String
                      )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Repeat String"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]
        ]


storyBool : Bulletproof.Story
storyBool =
    Bulletproof.folder "Bool"
        [ Bulletproof.story "Unchecked"
            (Knob.view viewport
                [ ( "Bool", Knob.Bool False )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Checked"
            (Knob.view viewport
                [ ( "Bool", Knob.Bool True )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyInt : Bulletproof.Story
storyInt =
    Bulletproof.folder "Int"
        [ Bulletproof.story "Input"
            (Knob.view viewport
                [ ( "Int", Knob.Int False 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Range"
            (Knob.view viewport
                [ ( "Int", Knob.Int True 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyFloat : Bulletproof.Story
storyFloat =
    Bulletproof.folder "Float"
        [ Bulletproof.story "Input"
            (Knob.view viewport
                [ ( "Float", Knob.Float False 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Range"
            (Knob.view viewport
                [ ( "Float", Knob.Float True 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyRadio : Bulletproof.Story
storyRadio =
    Bulletproof.folder "Radio"
        [ Bulletproof.story "Duplicates"
            (Knob.view viewport
                [ ( "Radio"
                  , Knob.Choice Knob.Radio
                        "Option #1"
                        [ "Option #1"
                        , "Option #1"
                        ]
                  )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Single"
            (Knob.view viewport
                [ ( "Radio"
                  , Knob.Choice Knob.Radio
                        "Option #1"
                        [ "Option #1"
                        ]
                  )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Long options"
            (\width n ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view viewport
                        [ ( "Radio"
                          , Knob.Choice Knob.Radio
                                "Option #1"
                                [ "Option #1"
                                , String.repeat n "ALongLine"
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Container Width"
                300
                [ Bulletproof.Knob.min 0
                ]
            |> Bulletproof.Knob.int "Repeat String"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]

        --
        , Bulletproof.story "Multiple"
            (\n ->
                Knob.view viewport
                    [ ( "Radio"
                      , List.range 1 n
                            |> List.map (\i -> "Option #" ++ String.fromInt i)
                            |> Knob.Choice Knob.Radio "Option #1"
                      )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Amount of Options"
                5
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 20
                ]
        ]


storySelect : Bulletproof.Story
storySelect =
    Bulletproof.folder "Select"
        [ Bulletproof.story "Duplicates"
            (Knob.view viewport
                [ ( "Select"
                  , Knob.Choice Knob.Select
                        "Option #1"
                        [ "Option #1"
                        , "Option #1"
                        ]
                  )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Single"
            (Knob.view viewport
                [ ( "Select"
                  , Knob.Choice Knob.Select
                        "Option #1"
                        [ "Option #1"
                        ]
                  )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Long options"
            (\width n ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view viewport
                        [ ( "Select"
                          , Knob.Choice Knob.Select
                                "Option #1"
                                [ "Option #1"
                                , String.repeat n "ALongLine"
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Container Width"
                300
                [ Bulletproof.Knob.min 0
                ]
            |> Bulletproof.Knob.int "Repeat String"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]

        --
        , Bulletproof.story "Multiple"
            (\n ->
                Knob.view viewport
                    [ ( "Select"
                      , List.range 1 n
                            |> List.map (\i -> "Option #" ++ String.fromInt i)
                            |> Knob.Choice Knob.Select "Option #1"
                      )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Amount of Options"
                5
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 20
                ]
        ]


story : Bulletproof.Story
story =
    Bulletproof.folder "Knob"
        [ Bulletproof.story "Empty"
            (Knob.view viewport [] Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.story "Single"
            (\name ->
                Knob.view viewport
                    [ ( name, Knob.Bool True )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Knob Name" "Something"

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
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Amount of Knobs"
                10
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 50
                ]

        --
        , Bulletproof.story "Color"
            (\r g b ->
                Knob.view
                    viewport
                    [ ( "Color", Knob.Color (Color.makeColor r g b) )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Red"
                0
                [ Bulletproof.Knob.range, Bulletproof.Knob.min 0, Bulletproof.Knob.max 255 ]
            |> Bulletproof.Knob.int "Green"
                0
                [ Bulletproof.Knob.range, Bulletproof.Knob.min 0, Bulletproof.Knob.max 255 ]
            |> Bulletproof.Knob.int "Blue"
                0
                [ Bulletproof.Knob.range, Bulletproof.Knob.min 0, Bulletproof.Knob.max 255 ]

        --
        , Bulletproof.story "Date"
            (\ms ->
                Knob.view
                    viewport
                    [ ( "Date", Knob.Date (Time.millisToPosix ms) )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Milliseconds" 0 [ Bulletproof.Knob.min 0 ]

        --
        , Bulletproof.story "Time"
            (\hours minutes ->
                Knob.view
                    viewport
                    [ ( "Time", Knob.Time (Date.Time hours minutes) )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Hours"
                0
                [ Bulletproof.Knob.range, Bulletproof.Knob.min 0, Bulletproof.Knob.max 23 ]
            |> Bulletproof.Knob.int "Minutes"
                0
                [ Bulletproof.Knob.range, Bulletproof.Knob.min 0, Bulletproof.Knob.max 59 ]

        --
        , Bulletproof.story "Files"
            (Knob.view
                viewport
                [ ( "Files", Knob.Files )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
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
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.bool "Use custom viewport" False
            |> Bulletproof.Knob.int "Width" viewport.width []
            |> Bulletproof.Knob.int "Height" viewport.height []
            |> Bulletproof.Knob.viewport

        --
        , storyString

        --
        , storyBool

        --
        , storyInt

        --
        , storyFloat

        --
        , storyRadio

        --
        , storySelect
        ]
