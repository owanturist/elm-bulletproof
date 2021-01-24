# Make your UI Bulletproof

Bulletproof is a tool which helps you to organize UI components isolated of each other.
Inspired by [Storybook](https://storybook.js.org) project.

```bash
elm install owanturist/elm-bulletproof
```


## Example and [Demo](http://bulletproof-demo.surge.sh)

```elm
module HelloWorld exposing (story)

import Html exposing (div, h1, p, text)
import Bulletproof
import Bulletproof.Knob


story : Bulletproof.Story
story =
    Bulletproof.story "Hello World"
        (\storyTitle storyText ->
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

```

## Setup

To setup Bulletproofs' app please take a look into [`demo`](https://github.com/owanturist/elm-bulletproof/tree/master/demo) folder.
There you might find files with exapmles of code required to run it.
I'm sorry you have to copy-paste the configuration.
It's under progress to get started by single command.


## Features

- Set of knobs to interact with component inputs dynamically
- Configurable layout of Bulletproofs' workspace
- Friendly warning messages when unexpected stories or knobs come to Bulletproof
- Hotkeys to configure the layout and navigate between stories
- Each story has uniq path to restore from url


## Ideology

There is no way to include a component messages to Elm loop. It meas that all components are static.
There is only one way to dynamically interact with them by knobs.
The reason is to simplify interface of story creation.
Otherwise a developer must to develop wide system of custom messages and models to make
[The Elm Architecture](https://guide.elm-lang.org/architecture) works.


## Roadmap

- [ ] Searching
- [ ] Restore knobs state from URL
- [ ] ~~Create npm package to avoid passing ports by hands. Should be something simillar with 
[elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest)~~
- [ ] ~~Mark todos on high level to indicate how much of them and where they exist~~
