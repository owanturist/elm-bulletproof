module Stories.Link exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html exposing (text)
import Link exposing (link)
import Router


story : Bulletproof.Story
story =
    Bulletproof.storyOf "Link"
        (\str ->
            link Router.ToNotFound
                []
                [ text str
                ]
                |> Bulletproof.html
        )
        |> Bulletproof.Knob.string "Text" "Simple Link"
