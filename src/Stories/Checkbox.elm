module Stories.Checkbox exposing (story)

import Bulletproof
import Bulletproof.Knob
import Checkbox exposing (checkbox)
import Html.Attributes


story : Bulletproof.Story
story =
    Bulletproof.storyOf "Checkbox"
        (\check ->
            checkbox [ Html.Attributes.checked check ]
                |> Bulletproof.html
        )
        |> Bulletproof.Knob.bool "Checked" False
