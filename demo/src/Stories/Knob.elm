module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Color
import Css
import Date
import Html.Styled exposing (div, styled)
import Knob
import Time


storyString : Bulletproof.Story
storyString =
    Bulletproof.folderOf "String"
        [ Bulletproof.storyOf "Empty"
            (Knob.view
                [ ( "String", Knob.String "" )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Single line"
            (Knob.view
                [ ( "String", Knob.String "A single line" )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Single long line"
            (\n ->
                Knob.view
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
        , Bulletproof.storyOf "Multiline"
            (\n ->
                Knob.view
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
    Bulletproof.folderOf "Bool"
        [ Bulletproof.storyOf "Unchecked"
            (Knob.view
                [ ( "Bool", Knob.Bool False )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Checked"
            (Knob.view
                [ ( "Bool", Knob.Bool True )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyInt : Bulletproof.Story
storyInt =
    Bulletproof.folderOf "Int"
        [ Bulletproof.storyOf "Input"
            (Knob.view
                [ ( "Int", Knob.Int False 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Range"
            (Knob.view
                [ ( "Int", Knob.Int True 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyFloat : Bulletproof.Story
storyFloat =
    Bulletproof.folderOf "Float"
        [ Bulletproof.storyOf "Input"
            (Knob.view
                [ ( "Float", Knob.Float False 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Range"
            (Knob.view
                [ ( "Float", Knob.Float True 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]


storyRadio : Bulletproof.Story
storyRadio =
    Bulletproof.folderOf "Radio"
        [ Bulletproof.storyOf "Duplicates"
            (Knob.view
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
        , Bulletproof.storyOf "Single"
            (Knob.view
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
        , Bulletproof.storyOf "Long options"
            (\width n ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view
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
        , Bulletproof.storyOf "Multiple"
            (\n ->
                Knob.view
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
    Bulletproof.folderOf "Select"
        [ Bulletproof.storyOf "Duplicates"
            (Knob.view
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
        , Bulletproof.storyOf "Single"
            (Knob.view
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
        , Bulletproof.storyOf "Long options"
            (\width n ->
                styled div
                    [ Css.width (Css.px (toFloat width))
                    , Css.backgroundColor (Css.hex "#ccc")
                    ]
                    []
                    [ Knob.view
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
        , Bulletproof.storyOf "Multiple"
            (\n ->
                Knob.view
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
    Bulletproof.folderOf "Knob"
        [ Bulletproof.storyOf "Empty"
            (Knob.view [] Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Duplicates"
            (Knob.view
                [ ( "Knob #2", Knob.Bool True )
                , ( "Knob #1", Knob.String "Text" )
                ]
                Knob.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Single"
            (\name ->
                Knob.view
                    [ ( name, Knob.Bool True )
                    ]
                    Knob.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Knob Name" "Something"

        --
        , Bulletproof.storyOf "Multiple"
            (\n ->
                Knob.view
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

        --
        , Bulletproof.storyOf "All Together"
            (Knob.view
                (List.reverse
                    [ ( "String", Knob.String "String Value" )
                    , ( "Bool", Knob.Bool True )
                    , ( "Int", Knob.Int False 10 { min = Nothing, max = Nothing, step = Nothing } )
                    , ( "Float", Knob.Float False 0.1 { min = Nothing, max = Nothing, step = Nothing } )
                    , ( "Int Range", Knob.Float True 1 { min = Just -10, max = Just 10, step = Just 2 } )
                    , ( "Float Range", Knob.Float True 0.5 { min = Just -1, max = Just 1, step = Just 0.5 } )
                    , ( "Radio", Knob.Choice Knob.Radio "first" [ "first", "second", "third" ] )
                    , ( "Select", Knob.Choice Knob.Select "first" [ "first", "second", "third" ] )
                    , ( "Color", Knob.Color (Color.makeColor "#ffff00" 255 255 0) )
                    , ( "Date", Knob.Date (Time.millisToPosix 0) )
                    , ( "Time", Knob.Time (Date.Time 2 20) )
                    , ( "Files", Knob.Files )
                    ]
                )
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]
