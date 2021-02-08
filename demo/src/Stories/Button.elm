module Stories.Button exposing (story)

import Bulletproof
import Bulletproof.Knob
import Button exposing (button)
import Html.Styled
import Icon


story : Bulletproof.Story
story =
    Bulletproof.story "Button"
        (\icon ->
            button ()
                []
                [ icon ]
        )
        |> Bulletproof.Knob.select "Icon"
            [ ( "folder", Icon.folder )
            , ( "folderOpen", Icon.folderOpen )
            ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
