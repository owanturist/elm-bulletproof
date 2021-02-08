module Stories.Empty exposing (story)

import Bulletproof
import Empty
import Html.Styled


story : Bulletproof.Story
story =
    Bulletproof.story "Empty"
        Empty.view
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
