module Main exposing (main)

import Browser
import Bulletproof
import Bulletproof.Knob
import Counter


main : Bulletproof.Program
main =
    Bulletproof.program
        [ (\count ->
            Counter.view count
          )
            |> Bulletproof.storyOf "Counter positive"
            |> Bulletproof.Knob.int "count" 10

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
