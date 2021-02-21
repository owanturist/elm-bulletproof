module Button exposing (button, css)

import Html exposing (Html, div)
import Html.Attributes as Attributes
import Html.Events as Events
import Palette
import Style
import Utils exposing (onSpaceOrEnter)


css : Style.Sheet
css =
    Style.elements
        [ button__root
        ]


button__root : Style.Element
button__root =
    Style.el "button__root"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "inline-flex"
        , Style.rule "align-items" "center"
        , Style.rule "justify-content" "center"
        , Style.rule "width" "24px"
        , Style.rule "height" "24px"
        , Style.rule "border" ("1px solid " ++ Palette.gray)
        , Style.rule "color" Palette.dark
        , Style.rule "background" Palette.white
        , Style.rule "border-radius" "3px"
        , Style.rule "font-size" "0"
        , Style.rule "outline" "none"
        , Style.rule "cursor" "pointer"
        , Style.rule "user-select" "none"
        ]
        |> Style.focusVisible
            [ Style.rule "box-shadow" ("0 0 0 2px " ++ Palette.gray05)
            ]


button : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
button onPress attributes =
    div
        (Style.class button__root
            :: Attributes.attribute "role" "button"
            :: Attributes.tabindex 0
            :: Events.onClick onPress
            :: onSpaceOrEnter onPress
            :: attributes
        )
