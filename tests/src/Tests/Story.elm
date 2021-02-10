module Tests.Story exposing (suite)

import Bulletproof
import Expect
import Story
import Test exposing (Test, test)


suite : Test
suite =
    Test.describe "Story.getFirst"
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
                ()
                    |> Bulletproof.story "story"
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
        , test "Nothing for empty in Bulletproof.folder"
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
