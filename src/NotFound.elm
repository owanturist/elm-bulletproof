module NotFound exposing (css, view)

import Html.Styled exposing (Html, br, code, div, text)
import Palette
import Story
import Style


css : Style.Sheet
css =
    Style.sheet
        [ not_found__path
        , not_found__message
        , not_found__root
        ]


not_found__path : Style.Selector
not_found__path =
    Style.class "not_found__path"
        [ Style.rule "display" "inline-block"
        , Style.rule "padding" "4px 12px"
        , Style.rule "background" Palette.gray05
        , Style.rule "border-radius" "4px"
        , Style.rule "font-family" "monospace"
        , Style.rule "letter-spacing" "0.05em"
        ]


not_found__message : Style.Selector
not_found__message =
    Style.class "not_found__message"
        [ Style.rule "padding" "12px 24px"
        ]


not_found__root : Style.Selector
not_found__root =
    Style.class "not_found__root"
        [ Style.rule "display" "flex"
        , Style.rule "align-items" "center"
        , Style.rule "justify-content" "center"
        , Style.rule "height" "100%"
        , Style.rule "width" "100%"
        , Style.rule "color" Palette.dark
        , Style.rule "background" Palette.white
        , Style.rule "line-height" "1.5"
        , Style.rule "font-size" "24px"
        , Style.rule "font-size" "3vim"
        , Style.rule "font-family" Palette.font
        ]


view : Story.Path -> Html msg
view path =
    div
        [ Style.className not_found__root
        ]
        [ div
            [ Style.className not_found__message
            ]
            [ text "Ooops..."
            , br [] []
            , text "There is no Story at this path:"
            , br [] []
            , code
                [ Style.className not_found__path ]
                [ text ("/ " ++ String.join " / " path) ]
            , br [] []
            , text "I suggest you to choose an existing one from the sidebar 😉"
            ]
        ]
