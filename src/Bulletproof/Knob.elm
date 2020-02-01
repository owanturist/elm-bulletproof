module Bulletproof.Knob exposing
    ( Color
    , Date
    , File
    , Property
    , Time
    , bool
    , color
    , date
    , files
    , float
    , int
    , max
    , min
    , radio
    , range
    , select
    , step
    , string
    , time
    )

import AVL.Dict as Dict
import Color
import Date
import Error
import File
import Knob exposing (Choice(..), Knob(..), Limits, Value(..), extract)
import Story


type alias Story view =
    Story.Story Error.Reason view


bool : String -> Bool -> Story (Bool -> a) -> Story a
bool name defaultValue story =
    case ( Error.validateNameOnly name, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue story =
    case ( Error.validateNameOnly name, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


type Property num
    = Range
    | Min num
    | Max num
    | Step num


range : Property num
range =
    Range


min : number -> Property number
min =
    Min


max : number -> Property number
max =
    Max


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


int : String -> Int -> List (Property Int) -> Story (Int -> a) -> Story a
int name defaultValue properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    case ( Error.validateInt name defaultValue limits, story ) of
        ( Err errors_, Story.Fail errors ) ->
            Story.Fail (errors_ ++ errors)

        ( Err errors, _ ) ->
            Story.Fail errors

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultValue properties story =
    let
        ( asRange, limits ) =
            propertiesToNumberPayload properties
    in
    case ( Error.validateFloat name defaultValue limits, story ) of
        ( Err errors_, Story.Fail errors ) ->
            Story.Fail (errors_ ++ errors)

        ( Err errors, _ ) ->
            Story.Fail errors

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


makeChoice : Choice -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice name options story =
    case ( Error.validateChoice choice name options, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            let
                optionsDict =
                    Dict.fromList options
            in
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


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
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


type alias Date =
    Date.Date


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue story =
    case ( Error.validateDate name defaultValue, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


type alias Time =
    Date.Time


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue story =
    case ( Error.validateTime name defaultValue, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors


type alias File =
    File.File


files : String -> Story (List File -> a) -> Story a
files name story =
    case ( Error.validateNameOnly name, story ) of
        ( Err error, Story.Fail errors ) ->
            Story.Fail (error :: errors)

        ( Err error, _ ) ->
            Story.Fail [ error ]

        ( Ok config, Story.Single storyID payload ) ->
            Story.Single storyID
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

        ( _, Story.Batch folderID stories ) ->
            Story.Batch folderID stories

        ( _, Story.Fail errors ) ->
            Story.Fail errors
