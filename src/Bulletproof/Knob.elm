module Bulletproof.Knob exposing
    ( bool, string
    , Property, min, max, step, range, int, float
    , radio, select
    , Color, Date, File, Time, color, date, files, time
    )

{-| Whant to add some dynamics to your stories?


# Primitives

@docs bool, string
@docs Property, min, max, step, range, int, float


# Customs

@docs radio, select


# Composites

@docs Color color
@docs Date date
@docs Time time
@docs File files

-}

import AVL.Dict as Dict
import Color
import Date
import Error
import File
import Knob exposing (Choice(..), Knob(..), Limits, Value(..), extract)
import Story


type alias Story view =
    Story.Story Error.Reason view


{-| Knob of a boolean value.

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
bool name defaultValue story =
    case ( Error.validateBool name, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Bool defaultValue ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (BoolValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


{-| Knob of a string value.

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
string name defaultValue story =
    case ( Error.validateString name, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, String defaultValue ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (StringValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


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

> Default for `int` and `float` ranges is `0`

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

> Defaults for `int` and `float` ranges are `100` and `1` respectively.

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

> Defaults for `int` and `float` ranges are `1` and `0.01` respectively.

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


{-| Knob of a string value.

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
int name defaultValue properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    case ( Error.validateInt name defaultValue limits, story ) of
        ( Err reasons_, Story.Fail title reasons ) ->
            Story.Fail title (reasons_ ++ reasons)

        ( Err reasons, Story.Single title _ ) ->
            Story.Fail title reasons

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Int asRange defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (IntValue val) ->
                                payload.view knobs (Maybe.withDefault defaultValue val)

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


{-| Knob of a string value.

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
float name defaultValue properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    case ( Error.validateFloat name defaultValue limits, story ) of
        ( Err reasons_, Story.Fail title reasons ) ->
            Story.Fail title (reasons_ ++ reasons)

        ( Err reasons, Story.Single title _ ) ->
            Story.Fail title reasons

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Float asRange defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (FloatValue val) ->
                                payload.view knobs (Maybe.withDefault defaultValue val)

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


makeChoice : Choice -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice name options story =
    case ( Error.validateChoice choice name options, story ) of
        ( Err reasons, Story.Fail title reasons_ ) ->
            Story.Fail title (reasons ++ reasons_)

        ( Err reasons, Story.Single title _ ) ->
            Story.Fail title reasons

        ( Ok config, Story.Single title payload ) ->
            let
                optionsDict =
                    Dict.fromList options
            in
            Story.Single title
                { knobs = ( config.name, Choice choice config.selected (List.map Tuple.first options) ) :: payload.knobs
                , view =
                    \knobs ->
                        let
                            value =
                                case extract config.name knobs of
                                    Just (StringValue key) ->
                                        Maybe.withDefault config.option (Dict.get key optionsDict)

                                    _ ->
                                        config.option
                        in
                        payload.view knobs value
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio =
    makeChoice Radio


select : String -> List ( String, option ) -> Story (option -> a) -> Story a
select =
    makeChoice Select


type alias Color =
    Color.Color


color : String -> String -> Story (Color -> a) -> Story a
color name defaultValue story =
    case ( Error.validateColor name defaultValue, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Color config.color ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (ColorValue (Just value)) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs config.color
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


type alias Date =
    Date.Date


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue story =
    case ( Error.validateDate name defaultValue, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Date config.date ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (DateValue (Just value)) ->
                                payload.view knobs (Date.dateFromPosix value)

                            _ ->
                                payload.view knobs (Date.dateFromPosix config.date)
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


type alias Time =
    Date.Time


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue story =
    case ( Error.validateTime name defaultValue, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Time config.time ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (TimeValue (Just value)) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs config.time
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons


type alias File =
    File.File


files : String -> Story (List File -> a) -> Story a
files name story =
    case ( Error.validateFile name, story ) of
        ( Err reason, Story.Fail title reasons ) ->
            Story.Fail title (reason :: reasons)

        ( Err reason, Story.Single title _ ) ->
            Story.Fail title [ reason ]

        ( Ok config, Story.Single title payload ) ->
            Story.Single title
                { knobs = ( config.name, Files ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract config.name knobs of
                            Just (FileValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs []
                }

        ( _, Story.Label title ) ->
            Story.Label title

        ( _, Story.Todo title ) ->
            Story.Todo title

        ( _, Story.Batch title stories ) ->
            Story.Batch title stories

        ( _, Story.Fail title reasons ) ->
            Story.Fail title reasons
