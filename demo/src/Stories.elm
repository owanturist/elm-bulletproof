port module Stories exposing (main)

import Bulletproof
import Stories.Button
import Stories.Error
import Stories.Icon
import Stories.Knob
import Stories.Menu
import Stories.Navigation
import Stories.Range


port save_settings : String -> Cmd msg


main : Bulletproof.Program
main =
    Bulletproof.program save_settings
        [ Stories.Icon.story
        , Stories.Button.story
        , Stories.Menu.story
        , Stories.Range.story
        , Stories.Navigation.story
        , Stories.Knob.story
        , Stories.Error.story
        ]
