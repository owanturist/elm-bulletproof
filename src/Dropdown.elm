module Dropdown exposing (dropdown, onMissClick)

import Browser.Events
import Css
import Html.Styled exposing (Html, div, styled)
import Html.Styled.Attributes as Attributes
import Palette
import Utils exposing (notClosest)


className : String
className =
    "__dropdown__"


styledMenu : List (Html msg) -> Html msg
styledMenu =
    styled div
        [ Css.position Css.absolute
        , Css.top (Css.pct 100)
        , Css.left Css.zero
        , Css.marginTop (Css.px 8)
        , Css.padding2 (Css.px 4) (Css.px 8)
        , Css.border3 (Css.px 1) Css.solid Palette.gray
        , Css.borderRadius (Css.px 3)
        , Css.backgroundColor Palette.white
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        ]
        []


styledDropdown : List (Html msg) -> Html msg
styledDropdown =
    styled div
        [ Css.position Css.relative
        ]
        [ Attributes.class className
        ]


dropdown : List (Html msg) -> Maybe (Html msg) -> Html msg
dropdown children menu =
    case menu of
        Nothing ->
            styledDropdown children

        Just menuNode ->
            styledDropdown (styledMenu [ menuNode ] :: children)


onMissClick : msg -> Sub msg
onMissClick msg =
    Browser.Events.onClick (notClosest className msg)
