module Bulletproof exposing
    ( Story, story, folder, todo, label
    , Renderer, fromHtml, fromElmCss, fromElmUI
    , Program, program
    )

{-| Basic API to create and organize static stories.


# Tell a story

@docs Story, story, folder, todo, label


# Render a component into a story

@docs Renderer, fromHtml, fromElmCss, fromElmUI


# Run a programm

@docs Program, program

-}

import Element exposing (Element)
import Error
import Html
import Html.Styled
import Main
import Renderer
import Story exposing (Story)


{-| Specific custom type incapsulates work with generic messages of components.
-}
type alias Renderer =
    Renderer.Renderer


{-| Allows your `Html.Styled` component to be rendered in Bulletproof.

> **Note:** In case of `elm-css` compatibility issues
> please convert `Html.Styled` to plain `Html` and use `fromHtml` then.

-}
fromElmCss : Html.Styled.Html msg -> Renderer
fromElmCss layout =
    Renderer.Renderer (Html.Styled.map (always ()) layout)


{-| Allows your `Html` component to be rendered in Bulletproof.
-}
fromHtml : Html.Html msg -> Renderer
fromHtml layout =
    fromElmCss (Html.Styled.fromUnstyled layout)


{-| Allows your `Element` component to be rendered in Bulletproof.

> **Note:** In case of `elm-ui` compatibility issues
> please convert `Element` to plain `Html` and use `fromHtml` then.

-}
fromElmUI : List Element.Option -> List (Element.Attribute msg) -> Element msg -> Renderer
fromElmUI options attributes element =
    fromHtml (Element.layoutWith { options = options } attributes element)


{-| Bulletproof is made of stories.
Stories helps you to organize UI components and describe different states.
-}
type alias Story =
    Story.Story Error.Reason Renderer


{-| Story represents a component according inputs.
To dynamically change the inputs please take a look into knobs.

    staticStory : Bulletproof.Story
    staticStory =
        Bulletproof.story "Simple static story"
            (button
                []
                [ text "Funny Button" ]
                |> Bulletproof.fromHtml
            )

-}
story : String -> view -> Story.Story Error.Reason view
story title view =
    Story.Single title
        { knobs = []
        , view = always view
        }


{-| Folder organizes your stories.
A folder might includes stories, todos, labels and other folders

    someFolder : Bulletproof.Story
    someFolder =
        Bulletproof.folder "Button"
            [ Bulletproof.story "default"
                (button
                    []
                    [ text "Button Label" ]
                )
                |> Bulletproof.fromHtml

            --
            , Bulletproof.story "disabled"
                (button
                    [ disabled True ]
                    [ text "Button Label" ]
                )
                |> Bulletproof.fromHtml
            ]

-}
folder : String -> List Story -> Story
folder title stories =
    Story.Batch title stories


{-| Each todo is a story which has not started yet...
Helps to remember components' states you want to make as a story.

    newComponent : Bulletproof.Story
    newComponent =
        Bulletproof.folder "New component even without a name"
            [ Bulletproof.todo "disabled"
            , Bulletproof.todo "loading"
            , Bulletproof.todo "failed"
            ]

-}
todo : String -> Story
todo title =
    Story.Todo title


{-| Labels helps to visually split stories by blocks. Does not affect on story path.
-}
label : String -> Story
label title =
    Story.Label title


{-| Specific Bulletproof program to return as main.
-}
type alias Program =
    Main.Program


{-| Program to represent your stories.

> **Note:** To run a program you have to pass port to work with localStorage.
> I'm sorry you have to do so but it's not forever.

-}
program : (String -> Cmd msg) -> List Story -> Program
program onSettingsChange stories =
    Main.run onSettingsChange stories
