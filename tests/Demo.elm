module Demo exposing (main)

import Bulletproof
import Stories.Button
import Stories.Icon
import Stories.Navigation
import Stories.Range


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Stories.Icon.story
        , Stories.Button.story
        , Stories.Range.story
        , Stories.Navigation.story
        ]
