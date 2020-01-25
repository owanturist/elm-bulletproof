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
        [ Bulletproof.storyOf "empty"
            (Navigation.view [] [] Navigation.initial
                |> Bulletproof.fromElmCss
            )
        , Bulletproof.folderOf "Stories"
            [ Bulletproof.storyOf "single"
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
            , Bulletproof.storyOf "multiple"
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
                    [ Bulletproof.Knob.range True
                    , Bulletproof.Knob.min 1
                    , Bulletproof.Knob.max 20
                    ]
            ]

        --
        , Bulletproof.folderOf "Folders"
            [ Bulletproof.storyOf "single"
                (\str ->
                    Navigation.view
                        []
                        [ Bulletproof.folderOf str []
                        ]
                        Navigation.initial
                        |> Bulletproof.fromElmCss
                )
                |> Bulletproof.Knob.string "Folder title" "single folder title"

            --
            , Bulletproof.storyOf "multiple"
                (\n ->
                    Navigation.view
                        []
                        (List.map
                            (\i -> Bulletproof.folderOf ("Folder #" ++ String.fromInt i) [])
                            (List.range 1 n)
                        )
                        Navigation.initial
                        |> Bulletproof.fromElmCss
                )
                |> Bulletproof.Knob.int "Amount of Folders"
                    3
                    [ Bulletproof.Knob.range True
                    , Bulletproof.Knob.min 1
                    , Bulletproof.Knob.max 20
                    ]

            --
            , Bulletproof.storyOf "nested"
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
                        |> Navigation.view (String.split " " current)
                            [ Bulletproof.folderOf "First"
                                [ Bulletproof.folderOf "Second"
                                    [ Bulletproof.folderOf "Third"
                                        [ Bulletproof.storyOf "Story1" dummy
                                        , Bulletproof.storyOf "Story2" dummy
                                        , Bulletproof.storyOf "Story3" dummy
                                        ]
                                    ]
                                ]
                            ]
                        |> Bulletproof.fromElmCss
                )
                |> Bulletproof.Knob.bool "Open First" True
                |> Bulletproof.Knob.bool "Open Second" True
                |> Bulletproof.Knob.bool "Open Third" True
                |> Bulletproof.Knob.string "Current Path" "First Second Third Story1"
            ]
        ]
