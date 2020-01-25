module Stories.Navigation exposing (story)

import AVL.Set as Set
import Bulletproof
import Bulletproof.Knob
import Html exposing (text)
import Navigation


dummy : Bulletproof.Renderer
dummy =
    Bulletproof.fromHtml (text "")


story : Bulletproof.Story
story =
    Bulletproof.folderOf "Navigation"
        [ Bulletproof.storyOf "Empty"
            (Navigation.view [] [] Navigation.initial
                |> Bulletproof.fromElmCss
            )
        , storyStories
        , storyFolders
        , storyLabels
        ]


storyStories : Bulletproof.Story
storyStories =
    Bulletproof.folderOf "Stories"
        [ Bulletproof.storyOf "Single"
            (\str ->
                Navigation.view
                    []
                    [ Bulletproof.storyOf str dummy
                    ]
                    Navigation.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Story Tilte" "single story title"

        --
        , Bulletproof.storyOf "Multiple"
            (\n ->
                Navigation.view
                    []
                    (List.map
                        (\i ->
                            Bulletproof.storyOf ("Story #" ++ String.fromInt i) dummy
                        )
                        (List.range 1 n)
                    )
                    Navigation.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Amount of Stories"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 20
                ]
        ]


storyFolders : Bulletproof.Story
storyFolders =
    Bulletproof.folderOf "Folders"
        [ Bulletproof.storyOf "Empty"
            (\opened title ->
                Navigation.view
                    []
                    [ Bulletproof.folderOf title []
                    ]
                    (if opened then
                        Set.insert [ title ] Navigation.initial

                     else
                        Navigation.initial
                    )
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.bool "Folder opened" True
            |> Bulletproof.Knob.string "Folder Title" "Folder"

        --
        , Bulletproof.storyOf "Single"
            (\opened current ->
                Navigation.view
                    [ "Folder", "Story #" ++ String.fromInt current ]
                    [ Bulletproof.folderOf "Folder"
                        [ Bulletproof.storyOf "Story #1" dummy
                        , Bulletproof.storyOf "Story #2" dummy
                        , Bulletproof.storyOf "Story #3" dummy
                        ]
                    ]
                    (if opened then
                        Set.insert [ "Folder" ] Navigation.initial

                     else
                        Navigation.initial
                    )
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.bool "Folder opened" True
            |> Bulletproof.Knob.int "Active Story #"
                1
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 0
                , Bulletproof.Knob.max 3
                ]

        --
        , Bulletproof.storyOf "Multiple"
            (\n ->
                Navigation.view
                    []
                    (List.map
                        (\i ->
                            Bulletproof.folderOf ("Folder #" ++ String.fromInt i)
                                [ Bulletproof.storyOf "Story" dummy
                                ]
                        )
                        (List.range 1 n)
                    )
                    Navigation.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.int "Amount of Folders"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 20
                ]

        --
        , Bulletproof.storyOf "Nested"
            (\openFirst openSecond openThird current ->
                [ ( openFirst, [ "First" ] )
                , ( openSecond, [ "First", "Second" ] )
                , ( openThird, [ "First", "Second", "Third" ] )
                ]
                    |> List.foldl
                        (\( open, path ) acc ->
                            if open then
                                Set.insert (List.reverse path) acc

                            else
                                acc
                        )
                        Navigation.initial
                    |> Navigation.view [ "First", "Second", "Third", "Story #" ++ String.fromInt current ]
                        [ Bulletproof.folderOf "First"
                            [ Bulletproof.folderOf "Second"
                                [ Bulletproof.folderOf "Third"
                                    [ Bulletproof.storyOf "Story #1" dummy
                                    , Bulletproof.storyOf "Story #2" dummy
                                    , Bulletproof.storyOf "Story #3" dummy
                                    ]
                                ]
                            ]
                        ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.bool "Open First" True
            |> Bulletproof.Knob.bool "Open Second" True
            |> Bulletproof.Knob.bool "Open Third" True
            |> Bulletproof.Knob.int "Active Story #"
                1
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 0
                , Bulletproof.Knob.max 3
                ]
        ]


storyLabels : Bulletproof.Story
storyLabels =
    Bulletproof.folderOf "Labels"
        [ Bulletproof.storyOf "Alone"
            (\title ->
                Navigation.view []
                    [ Bulletproof.label title
                    ]
                    Navigation.initial
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Label Title" "Standalone"

        --
        , Bulletproof.storyOf "Stories around"
            (Navigation.view []
                [ Bulletproof.label "Label #1"
                , Bulletproof.storyOf "Some story" dummy
                , Bulletproof.label "Label #2"
                , Bulletproof.folderOf "Empty Folder" []
                , Bulletproof.label "Label #3"
                , Bulletproof.folderOf "Folder"
                    [ Bulletproof.storyOf "Another story" dummy
                    ]
                ]
                Navigation.initial
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Nested"
            (\open5 ->
                Navigation.view []
                    [ Bulletproof.storyOf "Story #1" dummy
                    , Bulletproof.storyOf "Story #2" dummy
                    , Bulletproof.label "Label #1"
                    , Bulletproof.folderOf "Folder #1"
                        [ Bulletproof.storyOf "Story #3" dummy
                        , Bulletproof.label "Label #2"
                        , Bulletproof.folderOf "Folder #2" []
                        , Bulletproof.folderOf "Folder #3"
                            [ Bulletproof.label "Label #3"
                            , Bulletproof.storyOf "Story #4" dummy
                            , Bulletproof.folderOf "Folder #4" []
                            ]
                        ]
                    , Bulletproof.folderOf "Folder #5"
                        [ Bulletproof.label "Label #4"
                        , Bulletproof.storyOf "Story #5" dummy
                        ]
                    ]
                    (List.foldl Navigation.open
                        Navigation.initial
                        [ [ "Folder #1", "Folder #3", "Folder #4" ]
                        , if open5 then
                            [ "Folder #5" ]

                          else
                            []
                        ]
                    )
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.bool "Open Folder #5" False
        ]
