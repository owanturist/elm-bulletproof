module Bulletproof exposing
    ( Story, story, folder, batch, todo, label
    , html, htmlFrom
    , Program, program
    , ProgramWithSettings, programWithSettings
    )

{-| Basic API to create and organize static stories.


# Describe a story

@docs Story, story, folder, batch, todo, label


# Transform Html

@docs html, htmlFrom


# Render a component into a story


# Run a programm

@docs Program, program

@docs ProgramWithSettings, programWithSettings

-}

import Html exposing (Html)
import Main
import Story


{-| Bulletproof is made of stories.
Stories helps you to organize UI components and describe different states.
-}
type alias Story =
    Story.Story (Html ())


{-| Transform stories `Html msg` content to `Html ()`.
We should unify message of `Html` components so we can put
them next to each other without extra mapping.

That's why it should hardcode `()` as a message for `Html`.

    buttonStory : Bulletproof.Story
    buttonStory =
        Bulletproof.story "Static funny button"
            (button
                []
                [ text "Funny Button" ]
            )
            |> Bulletproof.html

    buttonStories : Bulletproof.Story
    buttonStories =
        Bulletproof.folder "Button"
            [ Bulletproof.story "default"
                (button
                    []
                    [ text "Button Label" ]
                )

            --
            , Bulletproof.story "disabled"
                (button
                    [ disabled True ]
                    [ text "Button Label" ]
                )
            ]
            |> Bulletproof.html

    stories : Bulletproof.Story
    stories =
        Bulletproof.folder "Button"
            [ -- Navigation.view : Html Navigation.Msg
              Bulletproof.story "Navigation" Navigation.view
                |> Bulletproof.html

            -- Profile.view : Html Profile.Msg
            , Bulletproof.story "Profile" Profile.view
                |> Bulletproof.html

            -- Header.view : Html msg
            , Bulletproof.story "Header" Header.view
            ]

-}
html : Story.Story (Html msg) -> Story
html view =
    htmlFrom identity view


{-| Transforms stories content to `Html ()`.

    buttonStory : Bulletproof.Story
    buttonStory =
        Bulletproof.story "Static funny button"
            (Html.Styled.button
                []
                [ Html.Styled.text "Funny Button" ]
            )
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

    buttonStories : Bulletproof.Story
    buttonStories =
        Bulletproof.folder "Button"
            [ Bulletproof.story "default"
                (Html.Styled.button
                    []
                    [ Html.Styled.text "Button Label" ]
                )

            --
            , Bulletproof.story "disabled"
                (Html.Styled.button
                    [ disabled True ]
                    [ Html.Styled.text "Button Label" ]
                )
            ]
            |> Bulletproof.htmlFrom Html.Styled.toUnstyled

-}
htmlFrom : (view -> Html msg) -> Story.Story view -> Story
htmlFrom toHtml view =
    Story.map
        (\workspace ->
            { knobs = workspace.knobs
            , view = Maybe.map (Html.map (always ()) << toHtml) << workspace.view
            }
        )
        view


{-| Stories represent a component according inputs.
You can use `Bulletproof.Knob` to dynamically change the input.

    buttonStory : Bulletproof.Story
    buttonStory =
        Bulletproof.story "Static funny button"
            (button
                []
                [ text "Funny Button" ]
            )
            |> Bulletproof.html

-}
story : String -> view -> Story.Story view
story title view =
    Story.Single (String.trim title)
        { knobs = []
        , view = always (Just view)
        }


{-| Folders organize your stories.
A folder might include stories, todos, labels and other folders.

    buttonStories : Bulletproof.Story
    buttonStories =
        Bulletproof.folder "Button"
            [ Bulletproof.story "default"
                (button
                    []
                    [ text "Button Label" ]
                )

            --
            , Bulletproof.todo "hover"

            --
            , Bulletproof.story "disabled"
                (button
                    [ disabled True ]
                    [ text "Button Label" ]
                )
            ]
            |> Bulletproof.html

-}
folder : String -> List (Story.Story view) -> Story.Story view
folder title stories =
    Story.Folder (String.trim title) (batch stories)


{-| Batchs help to keep list of stories together
without putting them into a folder.

    roundButtonStories : Bulletproof.Story
    roundButtonStories =
        Bulletproof.batch
            [ Bulletproof.todo "default"
            , Bulletproof.todo "hover"
            , Bulletproof.todo "disabled"
            ]
            |> Bulletproof.html

    squareButtonStories : Bulletproof.Story
    squareButtonStories =
        Bulletproof.batch
            [ Bulletproof.todo "default"
            , Bulletproof.todo "hover"
            , Bulletproof.todo "disabled"
            ]
            |> Bulletproof.html

    buttonStories : Bulletproof.Story
    buttonStories =
        Bulletproof.folder "Button"
            [ Bulletproof.label "ROUND"
            , roundButtonStories
            , Bulletproof.label "SQUARE"
            , squareButtonStories
            ]

-}
batch : List (Story.Story view) -> Story.Story view
batch stories =
    case stories of
        [ single ] ->
            single

        many ->
            Story.Batch many


{-| Todos hold names for stories so you won't forget to describe it later.

    buttonStories : Bulletproof.Story
    buttonStories =
        Bulletproof.folder "Button"
            [ Bulletproof.todo "default"
            , Bulletproof.todo "hover"
            , Bulletproof.todo "disabled"
            ]
            |> Bulletproof.html

-}
todo : String -> Story.Story view
todo title =
    Story.Todo (String.trim title)


{-| Labels helps to visually split stories by blocks. Does not affect on story path.
-}
label : String -> Story.Story view
label title =
    Story.Label (String.trim title)


{-| Specific Bulletproof program to return as main.
-}
type alias Program =
    Main.Program ()


{-| Program to represent your stories.

> **Note:** If you want Bulletproof to keep settings
> after page reload please use `programWithSettings`.

    -- src/Stories.elm
    main : Bulletproof.Program
    main =
        Bulletproof.program
            [ Bulletproof.label "Button"
            , Bulletproof.todo "default"
            , Bulletproof.todo "hover"
            , Bulletproof.todo "disabled"
            ]

    -- src/index.js
    import { Elm } from './Stories.elm';

    Elm.Stories.init();

-}
program : List (Story.Story (Html msg)) -> Program
program stories =
    batch stories
        |> html
        |> Main.run
            { settingsFromFlags = always Nothing
            , onSettingsChange = always Cmd.none
            }


{-| Specific Bulletproof program to return as main.

It expects JSON string to be passed thru `settingsJson` flag
so this way Bulletproof could keep settings after page refresh.

-}
type alias ProgramWithSettings =
    Main.Program
        { settingsJson : Maybe String
        }


{-| Program to represent your stories.

It expects `onSettingsJsonChange` to save JSON string settings.
It can use `localStorage` or HTTP request port as a way to store the value.

    -- src/Stories.elm
    port save_settings : String -> Cmd msg

    main : Bulletproof.Program
    main =
        Bulletproof.programWithSettings
            { onSettingsJsonChange = save_settings
            }
            [ Bulletproof.label "Button"
            , Bulletproof.todo "default"
            , Bulletproof.todo "hover"
            , Bulletproof.todo "disabled"
            ]

    -- src/index.js
    import { Elm } from './Stories.elm';

    const SETTINGS_KEY = 'bf_settings'

    const { ports } = Elm.Stories.init({
        flags: {
            settingsJson: localStorage.getItem(SETTINGS_KEY)
        }
    });

    ports.save_settings.subscribe(settings => {
        localStorage.setItem(SETTINGS_KEY, settings)
    })

-}
programWithSettings :
    { onSettingsJsonChange : String -> Cmd Never
    }
    -> List (Story.Story (Html msg))
    -> ProgramWithSettings
programWithSettings { onSettingsJsonChange } stories =
    batch stories
        |> html
        |> Main.run
            { settingsFromFlags = .settingsJson
            , onSettingsChange = onSettingsJsonChange
            }
