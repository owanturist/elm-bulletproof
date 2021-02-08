module Stories.Menu exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html.Styled
import Menu
import Settings exposing (default)


story : Bulletproof.Story
story =
    Bulletproof.folder "Menu"
        [ Bulletproof.story "Closed"
            (Menu.view False Settings.default)

        --
        , Bulletproof.story "Opened"
            (\opened ->
                Menu.view opened Settings.default
            )
            |> Bulletproof.Knob.bool "Show menu" True

        --
        , Bulletproof.story "Opposite settings"
            (\navigationVisible dockVisible dockOrientation showGrid addPaddings darkBackground ->
                { default
                    | navigationVisible = navigationVisible
                    , dockVisible = dockVisible
                    , dockOrientation = dockOrientation
                    , addPaddings = addPaddings
                    , darkBackground = darkBackground
                    , showGrid = showGrid
                }
                    |> Menu.view True
            )
            |> Bulletproof.Knob.bool "Navigation Visible" False
            |> Bulletproof.Knob.bool "Dock Visible" False
            |> Bulletproof.Knob.radio "Dock Orientation"
                [ ( "Vertical", Settings.Vertical )
                , ( "Horizontal", Settings.Horizontal )
                ]
            |> Bulletproof.Knob.bool "Show Grid" True
            |> Bulletproof.Knob.bool "Add Paddings" True
            |> Bulletproof.Knob.bool "Dark Background" True
        ]
        |> Bulletproof.htmlFrom Html.Styled.toUnstyled
