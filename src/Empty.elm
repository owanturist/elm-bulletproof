module Empty exposing (css, view)

import Html exposing (Html, br, div, pre, text)
import Palette
import Style
import SyntaxHighlight


exampleGetStarted : String
exampleGetStarted =
    """
module HelloWorld exposing (story)

import Html exposing (div, h1, p, text)
import Bulletproof
import Bulletproof.Knob


story : Bulletproof.Story
story =
    Bulletproof.story "Hello World"
        (\\storyTitle storyText ->
            div []
                [ h1 [] [ text storyTitle ]
                , p [] [ text storyText ]
                ]
                |> Bulletproof.fromHtml
        )
        |> Bulletproof.Knob.string "Story title" "Hello World"
        |> Bulletproof.Knob.radio "Story text"
            [ ( "Never ending story", "I once brought a honeycomb and a jackass into a brothel..." )
            , ( "Long story", "A long time ago in a galaxy far, far away..." )
            ]

    """


css : Style.Sheet
css =
    Style.elements
        [ empty__root
        , empty__container
        , empty__title
        ]


empty__root : Style.Element
empty__root =
    Style.el "empty__root"
        [ Style.rule "display" "flex"
        , Style.rule "align-items" "center"
        , Style.rule "justify-content" "center"
        , Style.rule "height" "100%"
        , Style.rule "width" "100%"
        , Style.rule "overflow" "auto"
        , Style.rule "color" Palette.dark
        , Style.rule "background" Palette.white
        , Style.rule "font-size" "13px"
        , Style.rule "font-size" "1.5vmin"
        , Style.rule "font-family" Palette.font
        ]


empty__container : Style.Element
empty__container =
    Style.el "empty__container"
        [ Style.rule "min-width" "0"
        , Style.rule "padding" "12px 24px"
        ]


empty__title : Style.Element
empty__title =
    Style.el "empty__title"
        [ Style.rule "margin-bottom" "1em"
        , Style.rule "font-size" "2.5em"
        ]


view : Html msg
view =
    div
        [ Style.class empty__root
        ]
        [ div
            [ Style.class empty__container
            ]
            [ div
                [ Style.class empty__title
                ]
                [ text "Don't know where to start?"
                , br [] []
                , text "Let me tell you a story..."
                ]
            , case SyntaxHighlight.elm (String.trim exampleGetStarted) of
                Err _ ->
                    pre [] [ text (String.trim exampleGetStarted) ]

                Ok elmCode ->
                    SyntaxHighlight.toBlockHtml Nothing elmCode
            ]
        ]
