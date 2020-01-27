module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Color
import Date
import Knob
import Time


story : Bulletproof.Story
story =
    Bulletproof.folderOf "Knob"
        [ Bulletproof.storyOf "Empty"
            (Knob.view [] Knob.initial
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
        , Bulletproof.storyOf "All Together"
            (Knob.view
                (List.reverse
                    [ ( "String", Knob.String "String Value" )
                    , ( "Bool", Knob.Bool True )
                    , ( "Int", Knob.Int False 10 { min = Nothing, max = Nothing, step = Nothing } )
                    , ( "Float", Knob.Float False 0.1 { min = Nothing, max = Nothing, step = Nothing } )
                    , ( "Int Range", Knob.Float True 1 { min = Just -10, max = Just 10, step = Just 2 } )
                    , ( "Float Range", Knob.Float True 0.5 { min = Just -1, max = Just 1, step = Just 0.5 } )
                    , ( "Radio", Knob.Choice Knob.Radio [ "first", "second", "third" ] )
                    , ( "Select", Knob.Choice Knob.Select [ "first", "second", "third" ] )
                    , ( "Color", Knob.Color (Color.fromString "#ff0") )
                    , ( "Date", Knob.Date (Just (Time.millisToPosix 0)) )
                    , ( "Time", Knob.Time (Date.timeFromString "16:00") )
                    , ( "Files", Knob.Files )
                    ]
                )
                Knob.initial
                |> Bulletproof.fromElmCss
            )
        ]
