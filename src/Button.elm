module Button exposing (button)

import Css
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Palette
import Utils exposing (onSpaceOrEnter)


cssButton : Bool -> List Css.Style
cssButton dark =
    let
        { color, background, border, shadow } =
            if dark then
                { color = Palette.white
                , background = Palette.dark
                , border = Palette.black
                , shadow = Palette.gray
                }

            else
                { color = Palette.dark
                , background = Palette.white
                , border = Palette.gray
                , shadow = Palette.gray50
                }
    in
    [ Css.boxSizing Css.borderBox
    , Css.display Css.inlineFlex
    , Css.alignItems Css.center
    , Css.justifyContent Css.center
    , Css.width (Css.px 24)
    , Css.height (Css.px 24)
    , Css.border3 (Css.px 1) Css.solid border
    , Css.color color
    , Css.backgroundColor background
    , Css.borderRadius (Css.px 3)
    , Css.fontSize (Css.px 0)
    , Css.outline Css.none
    , Css.cursor Css.pointer
    , Css.property "user-select" "none"

    --
    , Css.focus
        [ Css.boxShadow5 Css.zero Css.zero Css.zero (Css.px 2) shadow
        ]

    --
    , Css.hover
        [ Css.boxShadow Css.none
        ]
    ]


button :
    { onPress : msg
    , dark : Bool
    }
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
button config attributes =
    div
        (Attributes.css (cssButton config.dark)
            :: Attributes.attribute "role" "button"
            :: Attributes.tabindex 0
            :: Events.onClick config.onPress
            :: onSpaceOrEnter config.onPress
            :: attributes
        )
