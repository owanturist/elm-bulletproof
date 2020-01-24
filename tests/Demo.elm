module Demo exposing (main)

import Bulletproof
import Stories.Checkbox
import Stories.Link
import Stories.Navigation
import Stories.Range


main : Bulletproof.Program
main =
    Bulletproof.program
        [ Stories.Link.story
        , Stories.Checkbox.story
        , Stories.Range.story
        , Stories.Navigation.story
        ]
