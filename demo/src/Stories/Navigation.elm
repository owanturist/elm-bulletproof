module Stories.Navigation exposing (story)

import Bulletproof
import Bulletproof.Knob
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Navigation
import Set


dummy : Html msg
dummy =
    text ""


story : Bulletproof.Story
story =
    Bulletproof.folder "Navigation"
        [ Bulletproof.story "Empty"
            (Navigation.view [] (Bulletproof.batch []) Navigation.initial)
            |> Bulletproof.html

        --
        , Bulletproof.story "Overflow"
            (\title count ->
                Navigation.view []
                    (List.map
                        (\i ->
                            Bulletproof.story (title ++ " #" ++ String.fromInt i) dummy
                        )
                        (List.range 1 count)
                        |> Bulletproof.batch
                    )
                    Navigation.initial
                    |> List.singleton
                    |> div
                        [ style "width" "150px"
                        , style "height" "300px"
                        , style "border" "1px solid #444"
                        , style "overflow" "auto"
                        ]
            )
            |> Bulletproof.Knob.string "Stories prefix" "SuperLongStoryTitle"
            |> Bulletproof.Knob.int "Amount of Stories"
                30
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 100
                ]
            |> Bulletproof.html

        --
        , storyStories
        , storyFolders
        , storyTodo
        , storyLabels
        ]


storyStories : Bulletproof.Story
storyStories =
    Bulletproof.folder "Stories"
        [ Bulletproof.story "Single"
            (\str ->
                Navigation.view
                    []
                    (Bulletproof.story str dummy)
                    Navigation.initial
            )
            |> Bulletproof.Knob.string "Story Tilte" "single story title"

        --
        , Bulletproof.story "Multiple"
            (\n ->
                Navigation.view
                    []
                    (List.map
                        (\i ->
                            Bulletproof.story ("Story #" ++ String.fromInt i) dummy
                        )
                        (List.range 1 n)
                        |> Bulletproof.batch
                    )
                    Navigation.initial
            )
            |> Bulletproof.Knob.int "Amount of Stories"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 50
                ]
        ]
        |> Bulletproof.html


storyFolders : Bulletproof.Story
storyFolders =
    Bulletproof.folder "Folders"
        [ Bulletproof.story "Empty"
            (\opened title ->
                Navigation.view
                    []
                    (Bulletproof.folder title [])
                    (if opened then
                        Set.insert [ title ] Navigation.initial

                     else
                        Navigation.initial
                    )
            )
            |> Bulletproof.Knob.bool "Folder opened" True
            |> Bulletproof.Knob.string "Folder Title" "Folder"

        --
        , Bulletproof.story "Single"
            (\opened current ->
                Navigation.view
                    [ "Folder", "Story #" ++ String.fromInt current ]
                    (Bulletproof.folder "Folder"
                        [ Bulletproof.story "Story #1" dummy
                        , Bulletproof.story "Story #2" dummy
                        , Bulletproof.story "Story #3" dummy
                        ]
                    )
                    (if opened then
                        Set.insert [ "Folder" ] Navigation.initial

                     else
                        Navigation.initial
                    )
            )
            |> Bulletproof.Knob.bool "Folder opened" True
            |> Bulletproof.Knob.radio "Active Story"
                [ ( "Story #1", 1 )
                , ( "Story #2", 2 )
                , ( "Story #3", 3 )
                , ( "Inactive", -1 )
                ]

        --
        , Bulletproof.story "Multiple"
            (\n ->
                Navigation.view
                    []
                    (List.map
                        (\i ->
                            Bulletproof.folder ("Folder #" ++ String.fromInt i)
                                [ Bulletproof.story "Story" dummy
                                ]
                        )
                        (List.range 1 n)
                        |> Bulletproof.batch
                    )
                    Navigation.initial
            )
            |> Bulletproof.Knob.int "Amount of Folders"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 1
                , Bulletproof.Knob.max 50
                ]

        --
        , Bulletproof.story "Nested"
            (\openFirst openSecond openThird active ->
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
                    |> Navigation.view [ "First", "Second", "Third", "Story #" ++ String.fromInt active ]
                        (Bulletproof.folder "First"
                            [ Bulletproof.folder "Second"
                                [ Bulletproof.folder "Third"
                                    [ Bulletproof.story "Story #1" dummy
                                    , Bulletproof.story "Story #2" dummy
                                    , Bulletproof.story "Story #3" dummy
                                    ]
                                ]
                            ]
                        )
            )
            |> Bulletproof.Knob.bool "Open First" True
            |> Bulletproof.Knob.bool "Open Second" True
            |> Bulletproof.Knob.bool "Open Third" True
            |> Bulletproof.Knob.radio "Active Story"
                [ ( "Story #1", 1 )
                , ( "Story #2", 2 )
                , ( "Story #3", 3 )
                , ( "Inactive", -1 )
                ]
        ]
        |> Bulletproof.html


storyTodo : Bulletproof.Story
storyTodo =
    Bulletproof.folder "Todos"
        [ Bulletproof.story "Alone"
            (\title ->
                Navigation.view []
                    (Bulletproof.todo title)
                    Navigation.initial
            )
            |> Bulletproof.Knob.string "Todo Title" "Standalone"

        --
        , Bulletproof.story "Stories around"
            (Navigation.view []
                ([ Bulletproof.todo "Todo #1"
                 , Bulletproof.story "Some story" dummy
                 , Bulletproof.todo "Todo #2"
                 , Bulletproof.folder "Empty Folder" []
                 , Bulletproof.todo "Todo #3"
                 , Bulletproof.folder "Folder"
                    [ Bulletproof.story "Another story" dummy
                    ]
                 ]
                    |> Bulletproof.batch
                )
                Navigation.initial
            )

        --
        , Bulletproof.story "Nested"
            (\open5 ->
                Navigation.view []
                    ([ Bulletproof.todo "Todo #0"
                     , Bulletproof.story "Story #1" dummy
                     , Bulletproof.story "Story #2" dummy
                     , Bulletproof.todo "Todo #1"
                     , Bulletproof.folder "Folder #1"
                        [ Bulletproof.story "Story #3" dummy
                        , Bulletproof.todo "Todo #2"
                        , Bulletproof.folder "Folder #2" []
                        , Bulletproof.folder "Folder #3"
                            [ Bulletproof.todo "Todo #3"
                            , Bulletproof.story "Story #4" dummy
                            , Bulletproof.folder "Folder #4" []
                            ]
                        ]
                     , Bulletproof.folder "Folder #5"
                        [ Bulletproof.todo "Todo #4"
                        , Bulletproof.story "Story #5" dummy
                        ]
                     ]
                        |> Bulletproof.batch
                    )
                    (List.foldl Navigation.open
                        Navigation.initial
                        [ [ "Folder #1", "Folder #3", "Folder #4" ]
                        , if open5 then
                            [ "Folder #5" ]

                          else
                            []
                        ]
                    )
            )
            |> Bulletproof.Knob.bool "Open Folder #5" False
        ]
        |> Bulletproof.html


storyLabels : Bulletproof.Story
storyLabels =
    Bulletproof.folder "Labels"
        [ Bulletproof.story "Alone"
            (\title ->
                Navigation.view []
                    (Bulletproof.label title)
                    Navigation.initial
            )
            |> Bulletproof.Knob.string "Label Title" "Standalone"

        --
        , Bulletproof.story "Stories around"
            (Navigation.view []
                ([ Bulletproof.label "Label #1"
                 , Bulletproof.story "Some story" dummy
                 , Bulletproof.label "Label #2"
                 , Bulletproof.folder "Empty Folder" []
                 , Bulletproof.label "Label #3"
                 , Bulletproof.folder "Folder"
                    [ Bulletproof.story "Another story" dummy
                    ]
                 ]
                    |> Bulletproof.batch
                )
                Navigation.initial
            )

        --
        , Bulletproof.story "Nested"
            (\open5 ->
                Navigation.view []
                    ([ Bulletproof.label "Label #0"
                     , Bulletproof.story "Story #1" dummy
                     , Bulletproof.story "Story #2" dummy
                     , Bulletproof.label "Label #1"
                     , Bulletproof.folder "Folder #1"
                        [ Bulletproof.story "Story #3" dummy
                        , Bulletproof.label "Label #2"
                        , Bulletproof.folder "Folder #2" []
                        , Bulletproof.folder "Folder #3"
                            [ Bulletproof.label "Label #3"
                            , Bulletproof.story "Story #4" dummy
                            , Bulletproof.folder "Folder #4" []
                            ]
                        ]
                     , Bulletproof.folder "Folder #5"
                        [ Bulletproof.label "Label #4"
                        , Bulletproof.story "Story #5" dummy
                        ]
                     ]
                        |> Bulletproof.batch
                    )
                    (List.foldl Navigation.open
                        Navigation.initial
                        [ [ "Folder #1", "Folder #3", "Folder #4" ]
                        , if open5 then
                            [ "Folder #5" ]

                          else
                            []
                        ]
                    )
            )
            |> Bulletproof.Knob.bool "Open Folder #5" False
        ]
        |> Bulletproof.html
