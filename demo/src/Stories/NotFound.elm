module Stories.NotFound exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html.Styled
import NotFound


story : Bulletproof.Story
story =
    Bulletproof.story "NotFound"
        (\path ->
            String.trim path
                |> String.split "/"
                |> List.map String.trim
                |> NotFound.view
        )
        |> Bulletproof.Knob.string "Path" "Folder 1 / Folder 2 / Folder 3 / Story"
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
