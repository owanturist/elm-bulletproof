module Tests.Color exposing (fromStringSuite, makeColorSuite)

import Color exposing (Color)
import Expect
import Test exposing (Test, describe, test)


makeColorSuite : Test
makeColorSuite =
    describe "Color.makeColor"
        [ test "rgb(0, 0, 0)"
            (\_ ->
                Color.makeColor 0 0 0
                    |> Expect.equal (Color "#000000" 0 0 0 0 0 0)
            )

        --
        , test "rgb(1, 1, 1)"
            (\_ ->
                Color.makeColor 1 1 1
                    |> Expect.equal (Color "#010101" 1 1 1 1 1 1)
            )

        --
        , test "rgb(10, 10, 10)"
            (\_ ->
                Color.makeColor 10 10 10
                    |> Expect.equal (Color "#0a0a0a" 10 10 10 10 10 10)
            )

        --
        , test "rgb(68, 68, 68)"
            (\_ ->
                Color.makeColor 68 68 68
                    |> Expect.equal (Color "#444444" 68 68 68 68 68 68)
            )

        --
        , test "rgb(255, 255, 255)"
            (\_ ->
                Color.makeColor 255 255 255
                    |> Expect.equal (Color "#ffffff" 255 255 255 255 255 255)
            )
        ]


fromStringSuite : Test
fromStringSuite =
    describe "Color.fromString"
        [ test "000"
            (\_ ->
                Color.fromString "000"
                    |> Expect.equal (Just (Color "#000000" 0 0 0 0 0 0))
            )

        --
        , test "#000"
            (\_ ->
                Color.fromString "#000"
                    |> Expect.equal (Just (Color "#000000" 0 0 0 0 0 0))
            )

        --
        , test "000000"
            (\_ ->
                Color.fromString "000000"
                    |> Expect.equal (Just (Color "#000000" 0 0 0 0 0 0))
            )

        --
        , test "#000000"
            (\_ ->
                Color.fromString "#000000"
                    |> Expect.equal (Just (Color "#000000" 0 0 0 0 0 0))
            )

        --
        , test "no alpha #00000000"
            (\_ ->
                Color.fromString "#00000000"
                    |> Expect.equal Nothing
            )

        --
        , test "no alpha #0000"
            (\_ ->
                Color.fromString "#0000"
                    |> Expect.equal Nothing
            )

        --
        , test "short #00"
            (\_ ->
                Color.fromString "#00"
                    |> Expect.equal Nothing
            )

        --
        , test "short #00000"
            (\_ ->
                Color.fromString "#00000"
                    |> Expect.equal Nothing
            )
        ]
