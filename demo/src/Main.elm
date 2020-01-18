module Main exposing (main)

import Browser
import Bulletproof
import Counter


main : Bulletproof.Program (Bulletproof.WithKnobs {})
main =
    Bulletproof.program
        { knobs = Bulletproof.initialKnobs
        }
        [ (\count ->
            Counter.view count
          )
            |> Bulletproof.storyOf "Counter positive"
            |> Bulletproof.int "count" 10

        --
        , Counter.view 0
            |> Bulletproof.storyOf "Counter initial"
        ]
