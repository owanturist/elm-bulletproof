module Main exposing (main)

import Browser
import Bulletproof
import Counter


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Bulletproof.storyOf "Counter initial" <|
            \_ ->
                Counter.view 0
        , Bulletproof.storyOf "Counter positive" <|
            \_ ->
                Counter.view 10
        ]
