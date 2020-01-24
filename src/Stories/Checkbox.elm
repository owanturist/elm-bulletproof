module Stories.Checkbox exposing (story)

import Bulletproof
import Bulletproof.Knob
import Checkbox exposing (checkbox)
import Html.Styled.Attributes as Attributes


story : Bulletproof.Story
story =
    Bulletproof.storyOf "Checkbox"
        (\check ->
            checkbox [ Attributes.checked check ]
                |> Bulletproof.fromElmCss
        )
        |> Bulletproof.Knob.bool "Checked" False
