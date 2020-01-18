module Main exposing (main)

import Browser
import Bulletproof
import Bulletproof.Knob
import Counter
import Html exposing (br, div, text)


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Bulletproof.storyOf "Counter positive"
            (\count fl str ->
                div []
                    [ text str
                    , br [] []
                    , text (String.fromFloat fl)
                    , Counter.view count
                    ]
            )
            |> Bulletproof.Knob.int "count" 10
            |> Bulletproof.Knob.float "float" 10
            |> Bulletproof.Knob.string "text" "Counter example!"

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
