module Stories.Button exposing (story)

import Bulletproof
import Bulletproof.Knob
import Button exposing (button)
import Icon


story : Bulletproof.Story
story =
    Bulletproof.story "Button"
        (\dark icon ->
            button ()
                []
                [ icon ]
                |> Bulletproof.fromElmCss
        )
        |> Bulletproof.Knob.bool "Dark" False
        |> Bulletproof.Knob.select "Icon"
            [ ( "Dock Horizontal", Icon.dockHorizontal )
            , ( "Dock Vertical", Icon.dockVertical )
            ]
