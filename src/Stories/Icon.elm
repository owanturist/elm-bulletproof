module Stories.Icon exposing (story)

import Bulletproof
import Bulletproof.Knob
import Css
import Html.Styled exposing (Html, div, styled)
import Icon


viewContainer : Css.Color -> List (Html msg) -> Html msg
viewContainer color =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.wrap
        , Css.color color
        ]
        []


viewBlock : Html msg -> Html msg
viewBlock child =
    styled div
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.width (Css.px 50)
        , Css.height (Css.px 50)
        ]
        []
        [ child ]


story : Bulletproof.Story
story =
    Bulletproof.storyOf "Icon"
        (\color ->
            [ Icon.folder
            , Icon.folderOpen
            , Icon.elm
            ]
                |> List.map viewBlock
                |> viewContainer (Css.hex color.hex)
                |> Bulletproof.fromElmCss
        )
        |> Bulletproof.Knob.color "Color" "#000"
