module Bulletproof.Knob exposing
    ( bool, string
    , Property, min, max, step, range, int, float
    , radio, select
    , Color, color
    , date
    , Time, time
    , File, files
    , Viewport, viewport
    )

{-| Whant to add some dynamics to your stories?


# Primitives

@docs bool, string
@docs Property, min, max, step, range, int, float


# Customs

@docs radio, select


# Composites

@docs Color, color
@docs Date, date
@docs Time, time
@docs File, files
@docs Viewport, viewport

-}

import Dict
import File
import Knob exposing (Knob, Limits)
import Story exposing (Story)
import Time


{-| Knob of a `Bool` value.

    storyButton : Bulletproof.Story
    storyButton =
        Bulletproof.story "Button"
            (\bool ->
                button
                    [ disabled bool ]
                    [ text "Sign In" ]
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.bool "disabled" True

-}
bool : String -> Bool -> Story (Bool -> a) -> Story a
bool name defaultBool story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Bool defaultBool ) :: payload.knobs
            , view = Knob.applyBool name defaultBool payload.view
            }
        )
        story


{-| Knob of a `String` value.

    storyButton : Bulletproof.Story
    storyButton =
        Bulletproof.story "Button"
            (\title ->
                button
                    []
                    [ text title ]
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.string "Title" "Sign In"

-}
string : String -> String -> Story (String -> a) -> Story a
string name defaultString story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.String defaultString ) :: payload.knobs
            , view = Knob.applyString name defaultString payload.view
            }
        )
        story


{-| Specific property to configurate numeric knobs.
-}
type Property num
    = Range
    | Min num
    | Max num
    | Step num


{-| Represent numeric knob as range but not input.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\int float ->
                input
                    [ size int
                    , style "width" (pct float)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.int "Size"
                10
                [ Bulletproof.Knob.range
                ]
            |> Bulletproof.Knob.float "Width"
                0.5
                [ Bulletproof.Knob.range
                ]

-}
range : Property num
range =
    Range


{-| Set minimum value for numeric knobs.

> **Note:** Default for `int` and `float` ranges is `0`

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\int float ->
                input
                    [ size int
                    , style "width" (pct float)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.int "Size"
                200
                [ Bulletproof.Knob.min 100
                ]
            |> Bulletproof.Knob.float "Width"
                0.5
                [ Bulletproof.Knob.min 0.25
                ]

-}
min : number -> Property number
min =
    Min


{-| Set maximum value for numeric knobs.

> **Note:** Defaults for `int` and `float` ranges are `100` and `1` respectively.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\int float ->
                input
                    [ size int
                    , style "width" (pct float)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.int "Size"
                200
                [ Bulletproof.Knob.max 1000
                ]
            |> Bulletproof.Knob.float "Width"
                0.5
                [ Bulletproof.Knob.max 0.75
                ]

-}
max : number -> Property number
max =
    Max


{-| Set step for numeric knobs.

> **Note:** Defaults for `int` and `float` ranges are `1` and `0.01` respectively.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\int float ->
                input
                    [ size int
                    , style "width" (pct float)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.int "Size"
                200
                [ Bulletproof.Knob.step 100
                ]
            |> Bulletproof.Knob.float "Width"
                0.5
                [ Bulletproof.Knob.step 0.1
                ]

-}
step : number -> Property number
step =
    Step


propertyToNumberPayload : Property number -> ( Bool, Limits number ) -> ( Bool, Limits number )
propertyToNumberPayload property ( asRange, limits ) =
    case property of
        Range ->
            ( True, limits )

        Min num ->
            ( asRange, { limits | min = Just num } )

        Max num ->
            ( asRange, { limits | max = Just num } )

        Step num ->
            ( asRange, { limits | step = Just num } )


propertiesToNumberPayload : List (Property number) -> ( Bool, Limits number )
propertiesToNumberPayload =
    List.foldl
        propertyToNumberPayload
        ( False, Limits Nothing Nothing Nothing )


{-| Knob of a `Int` value.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\int ->
                input
                    [ size int
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.int "Size" 10 []

-}
int : String -> Int -> List (Property Int) -> Story (Int -> a) -> Story a
int name defaultInt properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Int asRange defaultInt limits ) :: payload.knobs
            , view = Knob.applyInt name defaultInt payload.view
            }
        )
        story


{-| Knob of a `Float` value.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\float ->
                input
                    [ style "width" (pct float)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.float "Width" 0.5 []

-}
float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultFloat properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Float asRange defaultFloat limits ) :: payload.knobs
            , view = Knob.applyFloat name defaultFloat payload.view
            }
        )
        story


makeChoice : (List String -> Knob) -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice makeKnob name options story =
    let
        keys =
            List.map Tuple.first options

        defaultOption =
            Maybe.map Tuple.second (List.head options)

        optionsDict =
            Dict.fromList options
    in
    Story.map
        (\payload ->
            { knobs = ( name, makeKnob keys ) :: payload.knobs
            , view = Knob.applyChoice name defaultOption optionsDict payload.view
            }
        )
        story


{-| Knob of a custom value represented as radio group.
The first option is selected by default.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\inputType ->
                input
                    [ type_ (Maybe.withDefault "text" inputType)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.radio "Type"
                [ ( "none", Nothing )
                , ( "email", Just "email" )
                , ( "password", Just "password" )
                ]

-}
radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio name options story =
    makeChoice Knob.Radio name options story


{-| Knob of a custom value represented as select element.
The first option is selected by default.

    storyInput : Bulletproof.Story
    storyInput =
        Bulletproof.story "Input"
            (\inputType ->
                input
                    [ type_ (Maybe.withDefault "text" inputType)
                    ]
                    []
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.select "Type"
                [ ( "none", Nothing )
                , ( "email", Just "email" )
                , ( "password", Just "password" )
                ]

-}
select : String -> List ( String, option ) -> Story (option -> a) -> Story a
select name options story =
    makeChoice Knob.Select name options story


{-| Shape contains both hex and rgb components.
-}
type alias Color =
    { hex : String
    , red : Int
    , green : Int
    , blue : Int
    , r : Int
    , g : Int
    , b : Int
    }


{-| Knob of a `Color` value.

    storyButton : Bulletproof.Story
    storyButton =
        Bulletproof.story "Button"
            (\bgcolor ->
                button
                    [ style "background" bgcolor.hex ]
                    [ text "Sign In" ]
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.color "Background" "#444"

-}
color : String -> String -> Story (Color -> a) -> Story a
color name defaultHex story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Color defaultHex ) :: payload.knobs
            , view = Knob.applyColor name defaultHex payload.view
            }
        )
        story


{-| Shape contains year, month, day and `Time.Posix` values.
-}
type alias Date =
    { posix : Time.Posix
    , year : Int
    , month : Int
    , day : Int
    }


{-| Knob of a `Date` value.

    storyToday : Bulletproof.Story
    storyToday =
        Bulletproof.story "Today"
            (\date ->
                div
                    []
                    [ text "Today is: "
                    , text (String.fromInt date.day ++ " / ")
                    , text (String.fromInt date.month ++ " / ")
                    , text (String.fromInt date.year)
                    ]
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.date "Date" "31-12-1999"

-}
date : String -> String -> Story (Date -> a) -> Story a
date name defaultDate story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Date defaultDate ) :: payload.knobs
            , view = Knob.applyDate name defaultDate payload.view
            }
        )
        story


{-| Shape contains hours and minutes.
-}
type alias Time =
    { hours : Int
    , minutes : Int
    }


{-| Knob of a `Time` value.

    storyNow : Bulletproof.Story
    storyNow =
        Bulletproof.story "Now"
            (\time ->
                div
                    []
                    [ text ("Minutes: " ++ String.fromInt time.minutes)
                    , text " | "
                    , text ("Hours: " String.fromInt time.hours)
                    ]
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.time "Time" "14:01"

-}
time : String -> String -> Story (Time -> a) -> Story a
time name defaultTime story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Time defaultTime ) :: payload.knobs
            , view = Knob.applyTime name defaultTime payload.view
            }
        )
        story


{-| Alias for Elm `File` representation.
-}
type alias File =
    File.File


{-| Knob of a `List File` value.

    storyCountFiles : Bulletproof.Story
    storyCountFiles =
        Bulletproof.story "Count Files"
            (\files ->
                text (String.fromInt (List.length files) ++ " files are ready to upload")
                    |> Bulletproof.fromHtml
            )
            |> Bulletproof.Knob.files "Files"

-}
files : String -> Story (List File -> a) -> Story a
files name story =
    Story.map
        (\payload ->
            { knobs = ( name, Knob.Files ) :: payload.knobs
            , view = Knob.applyFiles name payload.view
            }
        )
        story


{-| Shape of story viewport dimension.
-}
type alias Viewport =
    { width : Int
    , height : Int
    }


{-| Knob of a story viewport.

Creates a knob with `"Viewport"` name.

    storyCountFiles : Bulletproof.Story
    storyCountFiles =
        Bulletproof.story "Count Files"
            (\viewport ->
                img
                    [ href "cat.jpg"
                    , if viewport.width > viewport.height then
                        style "width" "100%"

                      else
                        style "height" "100%"
                    ]
                    []
                    |> Bulletproof.fromHtml
            )

-}
viewport : Story (Viewport -> a) -> Story a
viewport story =
    Story.map
        (\payload ->
            { knobs = ( "Viewport", Knob.StoryViewport ) :: payload.knobs
            , view = Knob.applyViewport payload.view
            }
        )
        story
