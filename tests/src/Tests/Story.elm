module Tests.Story exposing (getFirstSuite, getNextSuite, getPrevSuite, getSuite)

import Bulletproof
import Dict
import Expect exposing (Expectation)
import Story
import Test exposing (Test, describe, test)


getFirstSuite : Test
getFirstSuite =
    describe "Story.getFirst"
        [ test "Nothing for Bulletproof.label"
            (\_ ->
                Bulletproof.label "label"
                    |> Story.getFirst
                    |> Expect.equal Nothing
            )

        --
        , test "Nothing for Bulletproof.todo"
            (\_ ->
                Bulletproof.todo "todo"
                    |> Story.getFirst
                    |> Expect.equal Nothing
            )

        --
        , test "Just for Bulletproof.story"
            (\_ ->
                Bulletproof.story "story" ()
                    |> Story.getFirst
                    |> Expect.equal (Just [ "story" ])
            )

        --
        , test "Nothing for empty Bulletproof.batch"
            (\_ ->
                Bulletproof.batch []
                    |> Story.getFirst
                    |> Expect.equal Nothing
            )

        --
        , test "Just for singleton Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story" ()
                    ]
                    |> Story.getFirst
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
                    |> Story.getFirst
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
                    |> Story.getFirst
                    |> Expect.equal (Just [ "story #1" ])
            )

        --
        , test "Nothing for empty Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder" []
                    |> Story.getFirst
                    |> Expect.equal Nothing
            )

        --
        , test "Just for singleton story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder"
                    [ Bulletproof.story "story" ()
                    ]
                    |> Story.getFirst
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
                    |> Story.getFirst
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
                    |> Story.getFirst
                    |> Expect.equal (Just [ "folder #1", "folder #2", "story #1" ])
            )
        ]


getNextSuite : Test
getNextSuite =
    describe "Story.getNext"
        [ test "Nothing for single Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getNext [ "story #1" ]
                        , Expect.equal Nothing << Story.getNext [ "story #2" ]
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
                        [ Expect.equal Nothing << Story.getNext [ "story #3" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getNext [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNext [ "story #2" ]
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
                        [ Expect.equal Nothing << Story.getNext [ "label #1" ]
                        , Expect.equal Nothing << Story.getNext [ "label #2" ]
                        , Expect.equal Nothing << Story.getNext [ "todo #1" ]
                        , Expect.equal Nothing << Story.getNext [ "todo #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getNext [ "story #1" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getNext [ "story #2" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNext [ "story #3" ]
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
                        [ Expect.equal (Just [ "story #2" ]) << Story.getNext [ "story #1" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getNext [ "story #2" ]
                        , Expect.equal (Just [ "folder #1", "story #1" ]) << Story.getNext [ "story #3" ]
                        , Expect.equal (Just [ "folder #1", "story #2" ]) << Story.getNext [ "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "folder #1", "story #1" ]) << Story.getNext [ "folder #1", "story #2" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "story #1" ]) << Story.getNext [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "story #3" ]) << Story.getNext [ "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #2", "story #1" ]) << Story.getNext [ "folder #1", "story #3" ]
                        , Expect.equal (Just [ "story #4" ]) << Story.getNext [ "folder #1", "folder #2", "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getNext [ "story #4" ]
                        ]
            )
        ]


getPrevSuite : Test
getPrevSuite =
    describe "Story.getPrev"
        [ test "Nothing for single Bulletproof.story"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story #1" ()
                    ]
                    |> Expect.all
                        [ Expect.equal Nothing << Story.getPrev [ "story #1" ]
                        , Expect.equal Nothing << Story.getPrev [ "story #2" ]
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
                        [ Expect.equal Nothing << Story.getPrev [ "story #3" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrev [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrev [ "story #2" ]
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
                        [ Expect.equal Nothing << Story.getPrev [ "label #1" ]
                        , Expect.equal Nothing << Story.getPrev [ "label #2" ]
                        , Expect.equal Nothing << Story.getPrev [ "todo #1" ]
                        , Expect.equal Nothing << Story.getPrev [ "todo #2" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getPrev [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrev [ "story #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrev [ "story #3" ]
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
                        [ Expect.equal (Just [ "story #4" ]) << Story.getPrev [ "story #1" ]
                        , Expect.equal (Just [ "story #1" ]) << Story.getPrev [ "story #2" ]
                        , Expect.equal (Just [ "story #2" ]) << Story.getPrev [ "story #3" ]
                        , Expect.equal (Just [ "story #3" ]) << Story.getPrev [ "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "story #1" ]) << Story.getPrev [ "folder #1", "story #2" ]
                        , Expect.equal (Just [ "folder #1", "story #2" ]) << Story.getPrev [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "folder #1", "story #1" ]) << Story.getPrev [ "folder #1", "folder #1", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #1", "story #1" ]) << Story.getPrev [ "folder #1", "story #3" ]
                        , Expect.equal (Just [ "folder #1", "story #3" ]) << Story.getPrev [ "folder #1", "folder #2", "story #1" ]
                        , Expect.equal (Just [ "folder #1", "folder #2", "story #1" ]) << Story.getPrev [ "story #4" ]
                        ]
            )
        ]


expectStoryView : Maybe view -> Maybe (Story.Workspace view) -> Expectation
expectStoryView expected actual =
    Maybe.andThen
        ((|>) { state = Dict.empty, viewport = { width = 0, height = 0 } } << .view)
        actual
        |> Expect.equal expected


getSuite : Test
getSuite =
    describe "Story.get"
        [ test "Nothing for Bulletproof.label"
            (\_ ->
                Bulletproof.label "label"
                    |> Story.get [ "label" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Nothing for Bulletproof.todo"
            (\_ ->
                Bulletproof.todo "todo"
                    |> Story.get [ "todo" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Just for Bulletproof.story"
            (\_ ->
                Bulletproof.story "story" 1
                    |> Expect.all
                        [ expectStoryView Nothing << Story.get [ "story #1" ]
                        , expectStoryView Nothing << Story.get [ "story", "folder" ]
                        , expectStoryView Nothing << Story.get [ "folder", "story" ]
                        , expectStoryView (Just 1) << Story.get [ "story" ]
                        ]
            )

        --
        , test "Empty Bulletproof.batch"
            (\_ ->
                Bulletproof.batch []
                    |> Story.get [ "story" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Single Bulletproof.story in Bulletproof.batch"
            (\_ ->
                Bulletproof.batch
                    [ Bulletproof.story "story" 1
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.get [ "batch" ]
                        , expectStoryView Nothing << Story.get [ "batch", "story" ]
                        , expectStoryView Nothing << Story.get [ "story", "batch" ]
                        , expectStoryView (Just 1) << Story.get [ "story" ]
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
                        [ expectStoryView Nothing << Story.get [ "story #5" ]
                        , expectStoryView Nothing << Story.get [ "story #1", "story #2" ]
                        , expectStoryView Nothing << Story.get [ "folder", "story #3" ]
                        , expectStoryView (Just 1) << Story.get [ "story #1" ]
                        , expectStoryView (Just 2) << Story.get [ "story #2" ]
                        , expectStoryView (Just 3) << Story.get [ "story #3" ]
                        , expectStoryView (Just 4) << Story.get [ "story #4" ]
                        ]
            )

        --
        , test "Empty Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder" []
                    |> Story.get [ "folder" ]
                    |> expectStoryView Nothing
            )

        --
        , test "Single Bulletproof.story in Bulletproof.folder"
            (\_ ->
                Bulletproof.folder "folder"
                    [ Bulletproof.story "story" 1
                    ]
                    |> Expect.all
                        [ expectStoryView Nothing << Story.get [ "folder" ]
                        , expectStoryView Nothing << Story.get [ "story" ]
                        , expectStoryView Nothing << Story.get [ "story", "folder" ]
                        , expectStoryView (Just 1) << Story.get [ "folder", "story" ]
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
                        [ expectStoryView Nothing << Story.get [ "story #5" ]
                        , expectStoryView Nothing << Story.get [ "story #1", "story #2" ]
                        , expectStoryView Nothing << Story.get [ "folder", "story #5" ]
                        , expectStoryView (Just 1) << Story.get [ "folder", "story #1" ]
                        , expectStoryView (Just 2) << Story.get [ "folder", "story #2" ]
                        , expectStoryView (Just 3) << Story.get [ "folder", "story #3" ]
                        , expectStoryView (Just 4) << Story.get [ "folder", "story #4" ]
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
                        [ expectStoryView (Just 1) << Story.get [ "story #1" ]
                        , expectStoryView (Just 2) << Story.get [ "story #2" ]
                        , expectStoryView (Just 3) << Story.get [ "story #3" ]
                        , expectStoryView (Just 4) << Story.get [ "folder #1", "story #1" ]
                        , expectStoryView (Just 5) << Story.get [ "folder #1", "story #2" ]
                        , expectStoryView (Just 6) << Story.get [ "folder #1", "folder #1", "folder #1", "story #1" ]
                        , expectStoryView (Just 7) << Story.get [ "folder #1", "folder #1", "story #1" ]
                        , expectStoryView (Just 8) << Story.get [ "folder #1", "story #3" ]
                        , expectStoryView (Just 9) << Story.get [ "folder #1", "folder #2", "story #1" ]
                        , expectStoryView (Just 10) << Story.get [ "story #4" ]
                        ]
            )
        ]
