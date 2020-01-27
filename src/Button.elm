module Button exposing (button)

import Css
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Palette
import Utils exposing (onSpaceOrEnter)


cssButton : List Css.Style
cssButton =
    [ Css.boxSizing Css.borderBox
    , Css.display Css.inlineFlex
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.width (Css.px 24)
    , Css.height (Css.px 24)
    , Css.border3 (Css.px 1) Css.solid Palette.gray
    , Css.color Palette.dark
    , Css.backgroundColor Palette.white
    , Css.borderRadius (Css.px 3)
    , Css.fontSize (Css.px 0)
    , Css.outline Css.none
    , Css.cursor Css.pointer
    , Css.property "user-select" "none"

    --
    , Css.focus
        [ Css.boxShadow5 Css.zero Css.zero Css.zero (Css.px 2) Palette.gray50
        ]

    --
    , Css.hover
        [ Css.boxShadow Css.none
        ]
    ]


button : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
button msg attributes =
    div
        (Attributes.css cssButton
            :: Attributes.attribute "role" "button"
            :: Attributes.tabindex 0
            :: Events.onClick msg
            :: onSpaceOrEnter msg
            :: attributes
        )
