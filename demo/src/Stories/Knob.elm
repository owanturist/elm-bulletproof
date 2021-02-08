module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Css
import Html.Styled exposing (div, styled)
import Knob


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
            )

        --
        , Bulletproof.story "Single line"
            (Knob.view viewport
                [ ( "String", Knob.String "A single line" )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Single long line"
            (\n ->
                Knob.view viewport
                    [ ( "String", Knob.String (String.repeat n "ASingleLongLine") )
                    ]
                    Knob.initial
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
            )
            |> Bulletproof.Knob.int "Repeat String"
                100
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 200
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyBool : Bulletproof.Story
storyBool =
    Bulletproof.folder "Bool"
        [ Bulletproof.story "Unchecked"
            (Knob.view viewport
                [ ( "Bool", Knob.Bool False )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Checked"
            (Knob.view viewport
                [ ( "Bool", Knob.Bool True )
                ]
                Knob.initial
            )
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyInt : Bulletproof.Story
storyInt =
    Bulletproof.folder "Int"
        [ Bulletproof.story "Input"
            (Knob.view viewport
                [ ( "Int", Knob.Int False 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Range"
            (Knob.view viewport
                [ ( "Int", Knob.Int True 0 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
            )
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyFloat : Bulletproof.Story
storyFloat =
    Bulletproof.folder "Float"
        [ Bulletproof.story "Input"
            (Knob.view viewport
                [ ( "Float", Knob.Float False 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Range"
            (Knob.view viewport
                [ ( "Float", Knob.Float True 0.21 { min = Nothing, max = Nothing, step = Nothing } )
                ]
                Knob.initial
            )
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storyRadio : Bulletproof.Story
storyRadio =
    Bulletproof.folder "Radio"
        [ Bulletproof.story "Duplicates"
            (Knob.view viewport
                [ ( "Radio"
                  , Knob.Radio [ "Option #1", "Option #1" ]
                  )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Single"
            (Knob.view viewport
                [ ( "Radio"
                  , Knob.Radio [ "Option #1" ]
                  )
                ]
                Knob.initial
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
                          , Knob.Radio
                                [ "Option #1"
                                , String.repeat n "ALongLine"
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
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
                            |> Knob.Radio
                      )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.int "Amount of Options"
                5
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 20
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


storySelect : Bulletproof.Story
storySelect =
    Bulletproof.folder "Select"
        [ Bulletproof.story "Duplicates"
            (Knob.view viewport
                [ ( "Select"
                  , Knob.Select [ "Option #1", "Option #1" ]
                  )
                ]
                Knob.initial
            )

        --
        , Bulletproof.story "Single"
            (Knob.view viewport
                [ ( "Select"
                  , Knob.Select [ "Option #1" ]
                  )
                ]
                Knob.initial
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
                          , Knob.Select
                                [ "Option #1"
                                , String.repeat n "ALongLine"
                                , "Option #3"
                                ]
                          )
                        ]
                        Knob.initial
                    ]
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
                            |> Knob.Select
                      )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.int "Amount of Options"
                5
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 20
                ]
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled


story : Bulletproof.Story
story =
    Bulletproof.folder "Knob"
        [ Bulletproof.story "Empty"
            (Knob.view viewport [] Knob.initial)
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

        --
        , Bulletproof.story "Single"
            (\name ->
                Knob.view viewport
                    [ ( name, Knob.Bool True )
                    ]
                    Knob.initial
            )
            |> Bulletproof.Knob.string "Knob Name" "Something"
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

        --
        , Bulletproof.story "Files"
            (Knob.view
                viewport
                [ ( "Files", Knob.Files )
                ]
                Knob.initial
            )
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

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
