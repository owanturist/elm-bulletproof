module Bulletproof.Knob exposing
    ( bool, string
    , NumericProperty, min, max, step, range, int, float
    , radio, select
    , Color, color
    , Date, date
    , Time, time
    , File, files
    , Viewport, viewport
    )

{-| Whant to add some dynamics to your stories?


# Primitives

@docs bool, string
@docs NumericProperty, min, max, step, range, int, float


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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Bool defaultBool ) :: payload.knobs
            , view = Knob.applyBool trimmedName defaultBool payload.view
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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.String defaultString ) :: payload.knobs
            , view = Knob.applyString trimmedName defaultString payload.view
            }
        )
        story


{-| Specific property to configurate numeric knobs.
-}
type NumericProperty num
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
range : NumericProperty num
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
min : number -> NumericProperty number
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
max : number -> NumericProperty number
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
step : number -> NumericProperty number
step =
    Step


propertyToNumberPayload : NumericProperty number -> ( Bool, Limits number ) -> ( Bool, Limits number )
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


propertiesToNumberPayload : List (NumericProperty number) -> ( Bool, Limits number )
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
int : String -> Int -> List (NumericProperty Int) -> Story (Int -> a) -> Story a
int name defaultInt properties story =
    let
        trimmedName =
            String.trim name

        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Int asRange defaultInt limits ) :: payload.knobs
            , view = Knob.applyInt trimmedName defaultInt payload.view
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
float : String -> Float -> List (NumericProperty Float) -> Story (Float -> a) -> Story a
float name defaultFloat properties story =
    let
        trimmedName =
            String.trim name

        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Float asRange defaultFloat limits ) :: payload.knobs
            , view = Knob.applyFloat trimmedName defaultFloat payload.view
            }
        )
        story


makeChoice : (List String -> Knob) -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice makeKnob name options story =
    let
        trimmedName =
            String.trim name

        trimmedOptions =
            List.map (Tuple.mapFirst String.trim) options

        keys =
            List.map Tuple.first trimmedOptions

        defaultOption =
            Maybe.map Tuple.second (List.head trimmedOptions)

        optionsDict =
            Dict.fromList trimmedOptions
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, makeKnob keys ) :: payload.knobs
            , view = Knob.applyChoice trimmedName defaultOption optionsDict payload.view
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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Color defaultHex ) :: payload.knobs
            , view = Knob.applyColor trimmedName defaultHex payload.view
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
Valid delimiters are `/`, `-` and `.`.
Valid date patterns are `dd/mm/yyyy` and `yyyy/mm/dd`.

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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Date defaultDate ) :: payload.knobs
            , view = Knob.applyDate trimmedName defaultDate payload.view
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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Time defaultTime ) :: payload.knobs
            , view = Knob.applyTime trimmedName defaultTime payload.view
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
    let
        trimmedName =
            String.trim name
    in
    Story.map
        (\payload ->
            { knobs = ( trimmedName, Knob.Files ) :: payload.knobs
            , view = Knob.applyFiles trimmedName payload.view
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
