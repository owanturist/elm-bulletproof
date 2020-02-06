module Empty exposing (view)

import Css
import Html.Styled as Html exposing (Html, br, div, pre, styled, text)
import Palette
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


styledTitle : List (Html msg) -> Html msg
styledTitle =
    styled div
        [ Css.marginBottom (Css.em 1)
        , Css.fontSize (Css.em 2.5)
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
        , Css.fontSize (Css.px 13)
        , Css.fontSize (Css.vmin 1.5)
        ]
        []


view : Html msg
view =
    styledRoot
        [ Html.fromUnstyled (SyntaxHighlight.useTheme SyntaxHighlight.gitHub)
        , styledMessage
            [ styledTitle
                [ text "Don't know where to start?"
                , br [] []
                , text "Let me tell you a classic story..."
                ]
            , case SyntaxHighlight.elm (String.trim exampleGetStarted) of
                Err _ ->
                    pre [] [ text (String.trim exampleGetStarted) ]

                Ok elmCode ->
                    Html.fromUnstyled (SyntaxHighlight.toBlockHtml Nothing elmCode)
            ]
        ]
