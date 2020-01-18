module Main exposing (main)

import Browser
import Bulletproof
import Bulletproof.Knob
import Counter
import Html exposing (div, text)


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Bulletproof.storyOf "Counter positive"
            (\count str ->
                div []
                    [ text str
                    , Counter.view count
                    ]
            )
            |> Bulletproof.Knob.int "count" 10
            |> Bulletproof.Knob.string "text" "Counter example!"

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
