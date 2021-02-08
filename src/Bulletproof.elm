module Bulletproof exposing
    ( Story, story, folder, todo, label
    , Program, program
    )

{-| Basic API to create and organize static stories.


# Tell a story

@docs Story, story, folder, todo, label


# Render a component into a story


# Run a programm

@docs Program, program

-}

import Html exposing (Html)
import Html.Styled
import Main
import Story


fromUnstyled : Story.Story (Html a) -> Story.Story (Html.Styled.Html a)
fromUnstyled =
    Story.map
        (\{ knobs, view } ->
            { knobs = knobs
            , view = \state viewport -> Maybe.map Html.Styled.fromUnstyled (view state viewport)
            }
        )


{-| Bulletproof is made of stories.
Stories helps you to organize UI components and describe different states.
-}
type alias Story =
    Story.Story (Html ())


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
story : String -> view -> Story.Story view
story title view =
    Story.Single (String.trim title)
        { knobs = []
        , view = \_ _ -> Just view
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
    Story.Batch (String.trim title) (List.map fromUnstyled stories)


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
    Story.Todo (String.trim title)


{-| Labels helps to visually split stories by blocks. Does not affect on story path.
-}
label : String -> Story
label title =
    Story.Label (String.trim title)


{-| Specific Bulletproof program to return as main.
-}
type alias Program =
    Main.Program


{-| Program to represent your stories.

> **Note:** To run a program you have to pass port to work with localStorage.

-}
program : (String -> Cmd msg) -> List Story -> Program
program onSettingsChange stories =
    Main.run onSettingsChange (List.map fromUnstyled stories)
