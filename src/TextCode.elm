module TextCode exposing (css, textCode)

import Html exposing (Html, code, text)
import Palette
import Style


css : Style.Sheet
css =
    Style.elements
        [ text_code__root
        ]


text_code__root : Style.Element
text_code__root =
    Style.el "text_code__root"
        [ Style.rule "display" "inline-block"
        , Style.rule "padding" "2px 4px"
        , Style.rule "background" Palette.cloud
        , Style.rule "font-family" "monospace"
        , Style.rule "letter-spacing" "0.05em"
        , Style.rule "line-height" "18px"
        ]


textCode : String -> Html msg
textCode =
    code
        [ Style.class text_code__root
        ]
        << List.singleton
        << text
