module Tests.Story exposing (getFirstPathSuite, getNextSuite, getPrevSuite, getWorkspaceSuite)

import Bulletproof
import Dict
import Expect exposing (Expectation)
import Story
import Test exposing (Test, describe, test)


getFirstPathSuite : Test
getFirstPathSuite =
    describe "Story.getFirstPath"
        [ test "Nothing for Bulletproof.label"
            (\_ ->
                Bulletproof.label "label"
                    |> Story.getFirstPath
                    |> Expect.equal Nothing
            )

        --
        , test "Nothing for Bulletproof.todo"
            (\_ ->
                Bulletproof.todo "todo"
                    |> Story.getFirstPath
                    |> Expect.equal Nothing
            )

        --
        , test "Just for Bulletproof.story"
            (\_ ->
                Bulletproof.story "story" ()
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "story" ])
            )

        --
        , test "Nothing for empty Bulletproof.batch"
            (\_ ->
                Bulletproof.batch []
                    |> Story.getFirstPath
                    |> Expect.equal Nothing
            )

        --
        , test "Just for singleton Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "story" ])
            )

        --
        , test "Just for first story in Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.label "label #1"
                    , Bulletproof.todo "todo #1"
                    , Bulletproof.batch
                        [ Bulletproof.label "label #2"
                        , Bulletproof.todo "todo #2"
                        ]
                    , Bulletproof.story "story #1" ()
                    , Bulletproof.story "story #2" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "story #1" ])
            )

        --
        , test "Just for nested first story in Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.label "label #1"
                    , Bulletproof.todo "todo #1"
                    , Bulletproof.batch
                        [ Bulletproof.label "label #2"
                        , Bulletproof.story "story #1" ()
                        , Bulletproof.todo "todo #2"
                        ]
                    , Bulletproof.story "story #2" ()
                    , Bulletproof.story "story #3" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "story #1" ])
            )

        --
        , test "Nothing for empty Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder" []
                    |> Story.getFirstPath
                    |> Expect.equal Nothing
            )

        --
        , test "Just for singleton story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder"
                    [ Bulletproof.story "story" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "folder", "story" ])
            )

        --
        , test "Just for first story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder #1"
                    [ Bulletproof.label "label #1"
                    , Bulletproof.todo "todo #1"
                    , Bulletproof.folder "folder #2"
                        [ Bulletproof.label "label #2"
                        , Bulletproof.todo "todo #2"
                        ]
                    , Bulletproof.story "story #1" ()
                    , Bulletproof.story "story #2" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "folder #1", "story #1" ])
            )

        --
        , test "Just for first nested story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder #1"
                    [ Bulletproof.label "label #1"
                    , Bulletproof.todo "todo #1"
                    , Bulletproof.folder "folder #2"
                        [ Bulletproof.label "label #2"
                        , Bulletproof.story "story #1" ()
                        , Bulletproof.todo "todo #2"
                        ]
                    , Bulletproof.story "story #2" ()
                    , Bulletproof.story "story #3" ()
                    ]
                    |> Story.getFirstPath
                    |> Expect.equal (Just [ "folder #1", "folder #2", "story #1" ])
            )
        ]


getNextSuite : Test
getNextSuite =
    describe "Story.getNextPath"
        [ test "Nothing for single Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getNextPath [ "story #1" ]
                        , Expect.equal Nothing << Story.getNextPath [ "story #2" ]
                        ]
            )

        --
        , test "Return next story path Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    , Bulletproof.story "story #2" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getNextPath [ "story #3" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getNextPath [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNextPath [ "story #2" ]
                        ]
            )

        --
        , test "Ignore Bulletproof.label and Bulletproof.todo"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    , Bulletproof.label "label #1"
                    , Bulletproof.batch
                        [ Bulletproof.label "label #2"
                        , Bulletproof.story "story #2" ()
                        , Bulletproof.todo "todo #1"
                        ]
                    , Bulletproof.story "story #3" ()
                    , Bulletproof.todo "todo #2"
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getNextPath [ "label #1" ]
                        , Expect.equal Nothing << Story.getNextPath [ "label #2" ]
                        , Expect.equal Nothing << Story.getNextPath [ "todo #1" ]
                        , Expect.equal Nothing << Story.getNextPath [ "todo #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getNextPath [ "story #1" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getNextPath [ "story #2" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNextPath [ "story #3" ]
                        ]
            )

        --
        , test "Travel thru Bulletproof.folder"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" 1
                    , Bulletproof.story "story #2" 2
                    , Bulletproof.story "story #3" 3
                    , Bulletproof.folder "folder #1"
                        [ Bulletproof.story "story #1" 4
                        , Bulletproof.story "story #2" 5
                        , Bulletproof.folder "folder #1"
                            [ Bulletproof.label "label #1"
                            , Bulletproof.batch
                                [ Bulletproof.folder "folder #1"
                                    [ Bulletproof.todo "todo #1"
                                    , Bulletproof.story "story #1" 6
                                    , Bulletproof.label "label #1"
                                    ]
                                , Bulletproof.story "story #1" 7
                                ]
                            ]
                        , Bulletproof.story "story #3" 8
                        , Bulletproof.folder "folder #2"
                            [ Bulletproof.story "story #1" 9
                            ]
                        ]
                    , Bulletproof.story "story #4" 10
                    ]
                    |> Expect.all
                        [ Expect.equal (Just [ "story #2" ]) << Story.getNextPath [ "story #1" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getNextPath [ "story #2" ]
                        , Expect.equal (Just [ "folder #1", "story #1" ]) << Story.getNextPath [ "story #3" ]
                        , Expect.equal (Just [ "folder #1", "story #2" ]) << Story.getNextPath [ "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "folder #1", "story #1" ]) << Story.getNextPath [ "folder #1", "story #2" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "story #1" ]) << Story.getNextPath [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "story #3" ]) << Story.getNextPath [ "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #2", "story #1" ]) << Story.getNextPath [ "folder #1", "story #3" ]
                        , Expect.equal (Just [ "story #4" ]) << Story.getNextPath [ "folder #1", "folder #2", "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNextPath [ "story #4" ]
                        ]
            )
        ]


getPrevSuite : Test
getPrevSuite =
    describe "Story.getPrevPath"
        [ test "Nothing for single Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getPrevPath [ "story #1" ]
                        , Expect.equal Nothing << Story.getPrevPath [ "story #2" ]
                        ]
            )

        --
        , test "Return prev story path Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    , Bulletproof.story "story #2" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getPrevPath [ "story #3" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrevPath [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrevPath [ "story #2" ]
                        ]
            )

        --
        , test "Ignore Bulletproof.label and Bulletproof.todo"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    , Bulletproof.label "label #1"
                    , Bulletproof.batch
                        [ Bulletproof.label "label #2"
                        , Bulletproof.story "story #2" ()
                        , Bulletproof.todo "todo #1"
                        ]
                    , Bulletproof.story "story #3" ()
                    , Bulletproof.todo "todo #2"
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getPrevPath [ "label #1" ]
                        , Expect.equal Nothing << Story.getPrevPath [ "label #2" ]
                        , Expect.equal Nothing << Story.getPrevPath [ "todo #1" ]
                        , Expect.equal Nothing << Story.getPrevPath [ "todo #2" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getPrevPath [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrevPath [ "story #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrevPath [ "story #3" ]
                        ]
            )

        --
        , test "Travel thru Bulletproof.folder"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" 1
                    , Bulletproof.story "story #2" 2
                    , Bulletproof.story "story #3" 3
                    , Bulletproof.folder "folder #1"
                        [ Bulletproof.story "story #1" 4
                        , Bulletproof.story "story #2" 5
                        , Bulletproof.folder "folder #1"
                            [ Bulletproof.label "label #1"
                            , Bulletproof.batch
                                [ Bulletproof.folder "folder #1"
                                    [ Bulletproof.todo "todo #1"
                                    , Bulletproof.story "story #1" 6
                                    , Bulletproof.label "label #1"
                                    ]
                                , Bulletproof.story "story #1" 7
                                ]
                            ]
                        , Bulletproof.story "story #3" 8
                        , Bulletproof.folder "folder #2"
                            [ Bulletproof.story "story #1" 9
                            ]
                        ]
                    , Bulletproof.story "story #4" 10
                    ]
                    |> Expect.all
                        [ Expect.equal (Just [ "story #4" ]) << Story.getPrevPath [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrevPath [ "story #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrevPath [ "story #3" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getPrevPath [ "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "story #1" ]) << Story.getPrevPath [ "folder #1", "story #2" ]
                        , Expect.equal (Just [ "folder #1", "story #2" ]) << Story.getPrevPath [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "folder #1", "story #1" ]) << Story.getPrevPath [ "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "story #1" ]) << Story.getPrevPath [ "folder #1", "story #3" ]
                        , Expect.equal (Just [ "folder #1", "story #3" ]) << Story.getPrevPath [ "folder #1", "folder #2", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #2", "story #1" ]) << Story.getPrevPath [ "story #4" ]
                        ]
            )
        ]


expectStoryView : Maybe view -> Maybe (Story.Workspace view) -> Expectation
expectStoryView expected actual =
    Maybe.andThen
        ((|>) { state = Dict.empty, viewport = { width = 0, height = 0 } } << .view)
        actual
        |> Expect.equal expected


getWorkspaceSuite : Test
getWorkspaceSuite =
    describe "Story.getWorkspace"
        [ test "Nothing for Bulletproof.label"
            (\_ ->
                Bulletproof.label "label"
                    |> Story.getWorkspace [ "label" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Nothing for Bulletproof.todo"
            (\_ ->
                Bulletproof.todo "todo"
                    |> Story.getWorkspace [ "todo" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Just for Bulletproof.story"
            (\_ ->
                Bulletproof.story "story" 1
                    |> Expect.all
                        [ expectStoryView Nothing << Story.getWorkspace [ "story #1" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story", "folder" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "folder", "story" ]
                        , expectStoryView (Just 1) << Story.getWorkspace [ "story" ]
                        ]
            )

        --
        , test "Empty Bulletproof.batch"
            (\_ ->
                Bulletproof.batch []
                    |> Story.getWorkspace [ "story" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Single Bulletproof.story in Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story" 1
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.getWorkspace [ "batch" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "batch", "story" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story", "batch" ]
                        , expectStoryView (Just 1) << Story.getWorkspace [ "story" ]
                        ]
            )

        --
        , test "Multiple Bulletproof.story in Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" 1
                    , Bulletproof.story "story #2" 2
                    , Bulletproof.story "story #3" 3
                    , Bulletproof.story "story #4" 4
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.getWorkspace [ "story #5" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story #1", "story #2" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "folder", "story #3" ]
                        , expectStoryView (Just 1) << Story.getWorkspace [ "story #1" ]
                        , expectStoryView (Just 2) << Story.getWorkspace [ "story #2" ]
                        , expectStoryView (Just 3) << Story.getWorkspace [ "story #3" ]
                        , expectStoryView (Just 4) << Story.getWorkspace [ "story #4" ]
                        ]
            )

        --
        , test "Empty Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder" []
                    |> Story.getWorkspace [ "folder" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Single Bulletproof.story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder"
                    [ Bulletproof.story "story" 1
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.getWorkspace [ "folder" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story", "folder" ]
                        , expectStoryView (Just 1) << Story.getWorkspace [ "folder", "story" ]
                        ]
            )

        --
        , test "Multiple Bulletproof.story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder"
                    [ Bulletproof.story "story #1" 1
                    , Bulletproof.story "story #2" 2
                    , Bulletproof.story "story #3" 3
                    , Bulletproof.story "story #4" 4
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.getWorkspace [ "story #5" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "story #1", "story #2" ]
                        , expectStoryView Nothing << Story.getWorkspace [ "folder", "story #5" ]
                        , expectStoryView (Just 1) << Story.getWorkspace [ "folder", "story #1" ]
                        , expectStoryView (Just 2) << Story.getWorkspace [ "folder", "story #2" ]
                        , expectStoryView (Just 3) << Story.getWorkspace [ "folder", "story #3" ]
                        , expectStoryView (Just 4) << Story.getWorkspace [ "folder", "story #4" ]
                        ]
            )

        --
        , test "Deep nesting Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" 1
                    , Bulletproof.story "story #2" 2
                    , Bulletproof.story "story #3" 3
                    , Bulletproof.folder "folder #1"
                        [ Bulletproof.story "story #1" 4
                        , Bulletproof.story "story #2" 5
                        , Bulletproof.folder "folder #1"
                            [ Bulletproof.label "label #1"
                            , Bulletproof.batch
                                [ Bulletproof.folder "folder #1"
                                    [ Bulletproof.todo "todo #1"
                                    , Bulletproof.story "story #1" 6
                                    , Bulletproof.label "label #1"
                                    ]
                                , Bulletproof.story "story #1" 7
                                ]
                            ]
                        , Bulletproof.story "story #3" 8
                        , Bulletproof.folder "folder #2"
                            [ Bulletproof.story "story #1" 9
                            ]
                        ]
                    , Bulletproof.story "story #4" 10
                    ]
                    |> Expect.all
                        [ expectStoryView (Just 1) << Story.getWorkspace [ "story #1" ]
                        , expectStoryView (Just 2) << Story.getWorkspace [ "story #2" ]
                        , expectStoryView (Just 3) << Story.getWorkspace [ "story #3" ]
                        , expectStoryView (Just 4) << Story.getWorkspace [ "folder #1", "story #1" ]
                        , expectStoryView (Just 5) << Story.getWorkspace [ "folder #1", "story #2" ]
                        , expectStoryView (Just 6) << Story.getWorkspace [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , expectStoryView (Just 7) << Story.getWorkspace [ "folder #1", "folder #1", "story #1" ]
                        , expectStoryView (Just 8) << Story.getWorkspace [ "folder #1", "story #3" ]
                        , expectStoryView (Just 9) << Story.getWorkspace [ "folder #1", "folder #2", "story #1" ]
                        , expectStoryView (Just 10) << Story.getWorkspace [ "story #4" ]
                        ]
            )
        ]
