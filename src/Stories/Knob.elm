module Stories.Knob exposing (story)

import Bulletproof
import Bulletproof.Knob
import Knob


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
        ]
