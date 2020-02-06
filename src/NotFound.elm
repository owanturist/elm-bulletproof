module NotFound exposing (view)

import Css
import Html.Styled exposing (Html, br, code, div, styled, text)
import Palette
import Story


styledPath : List (Html msg) -> Html msg
styledPath =
    styled code
        [ Css.display Css.inlineBlock
        , Css.padding2 (Css.px 4) (Css.px 12)
        , Css.backgroundColor Palette.gray50
        , Css.borderRadius (Css.px 4)
        , Css.letterSpacing (Css.em 0.05)
        , Css.fontFamily Css.monospace
        ]
        []


styledMessage : List (Html msg) -> Html msg
styledMessage =
    styled div
        [ Css.padding2 (Css.px 12) (Css.px 24)
        ]
        []


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.height (Css.pct 100)
        , Css.width (Css.pct 100)
        , Css.color Palette.dark
        , Css.backgroundColor Palette.white
        , Css.fontFamilies Palette.font
        , Css.fontSize (Css.px 24)
        , Css.fontSize (Css.vmin 3)
        , Css.lineHeight (Css.num 1.5)
        ]
        []


view : Story.Path -> Html msg
view path =
    styledRoot
        [ styledMessage
            [ text "Ooops..."
            , br [] []
            , text "There is not Story at this path:"
            , br [] []
            , styledPath [ text (String.join " / " ("" :: path)) ]
            , br [] []
            , text "I suggest you to choose an existing one from the sidebar 😉"
            ]
        ]
