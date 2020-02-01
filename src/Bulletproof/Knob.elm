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

import Color
import Date
import Error
import File
import Knob exposing (Choice(..), Knob(..), Limits, Value(..), extract)
import Story exposing (Story(..))
import Utils exposing (nonBlank)


bool : String -> Bool -> Story (Bool -> a) -> Story a
bool name defaultValue story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            Single storyID
                { knobs = ( name, Bool defaultValue ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract name knobs of
                            Just (BoolValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultValue
                }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            Single storyID
                { knobs = ( name, String defaultValue ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract name knobs of
                            Just (StringValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultValue
                }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


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
    case ( nonBlank name, story ) of
        ( Nothing, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Single storyID payload ) ->
            let
                ( asRange, limits ) =
                    propertiesToNumberPayload properties
            in
            Single storyID
                { knobs = ( trimmedName, Int asRange defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract trimmedName knobs of
                            Just (IntValue val) ->
                                payload.view knobs (Maybe.withDefault defaultValue val)

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Label title ) ->
            Label title

        ( _, Todo title ) ->
            Todo title

        ( _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, Fail errors ) ->
            Fail errors


float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultValue properties story =
    case ( nonBlank name, story ) of
        ( Nothing, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Single storyID payload ) ->
            let
                ( asRange, limits ) =
                    propertiesToNumberPayload properties
            in
            Single storyID
                { knobs = ( trimmedName, Float asRange defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract trimmedName knobs of
                            Just (FloatValue val) ->
                                payload.view knobs (Maybe.withDefault defaultValue val)

                            _ ->
                                payload.view knobs defaultValue
                }

        ( _, Label title ) ->
            Label title

        ( _, Todo title ) ->
            Todo title

        ( _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, Fail errors ) ->
            Fail errors


makeChoice : Choice -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice name options story =
    case ( nonBlank name, List.head options, story ) of
        ( Nothing, _, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Nothing, Fail errors ) ->
            case choice of
                Radio ->
                    Fail (Error.EmptyRadio trimmedName :: errors)

                Select ->
                    Fail (Error.EmptySelect trimmedName :: errors)

        ( Just trimmedName, Nothing, _ ) ->
            case choice of
                Radio ->
                    Fail [ Error.EmptyRadio trimmedName ]

                Select ->
                    Fail [ Error.EmptySelect trimmedName ]

        ( Just trimmedName, Just ( firstLabel, firstValue ), Single storyID payload ) ->
            Single storyID
                { knobs = ( trimmedName, Choice choice firstLabel (List.map Tuple.first options) ) :: payload.knobs
                , view =
                    \knobs ->
                        let
                            selected =
                                case extract trimmedName knobs of
                                    Just (StringValue value) ->
                                        value

                                    _ ->
                                        firstLabel
                        in
                        options
                            |> List.filter ((==) selected << Tuple.first)
                            |> List.head
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault firstValue
                            |> payload.view knobs
                }

        ( _, _, Label title ) ->
            Label title

        ( _, _, Todo title ) ->
            Todo title

        ( _, _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, _, Fail errors ) ->
            Fail errors


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
    case ( nonBlank name, Color.fromString defaultValue, story ) of
        ( Nothing, _, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Nothing, Fail errors ) ->
            Fail (Error.InvalidColor trimmedName defaultValue :: errors)

        ( Just trimmedName, Nothing, _ ) ->
            Fail [ Error.InvalidColor trimmedName defaultValue ]

        ( Just trimmedName, Just defaultColor, Single storyID payload ) ->
            Single storyID
                { knobs = ( trimmedName, Color defaultColor ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract trimmedName knobs of
                            Just (ColorValue (Just value)) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultColor
                }

        ( _, _, Label title ) ->
            Label title

        ( _, _, Todo title ) ->
            Todo title

        ( _, _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, _, Fail errors ) ->
            Fail errors


type alias Date =
    Date.Date


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue story =
    case ( nonBlank name, Date.parseStringToPosix defaultValue, story ) of
        ( Nothing, _, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Nothing, Fail errors ) ->
            Fail (Error.InvalidDate trimmedName defaultValue :: errors)

        ( Just trimmedName, Nothing, _ ) ->
            Fail [ Error.InvalidDate trimmedName defaultValue ]

        ( Just trimmedName, Just defaultDate, Single storyID payload ) ->
            Single storyID
                { knobs = ( trimmedName, Date defaultDate ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract trimmedName knobs of
                            Just (DateValue (Just value)) ->
                                payload.view knobs (Date.dateFromPosix value)

                            _ ->
                                payload.view knobs (Date.dateFromPosix defaultDate)
                }

        ( _, _, Label title ) ->
            Label title

        ( _, _, Todo title ) ->
            Todo title

        ( _, _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, _, Fail errors ) ->
            Fail errors


type alias Time =
    Date.Time


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue story =
    case ( nonBlank name, Date.timeFromString defaultValue, story ) of
        ( Nothing, _, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( Just trimmedName, Nothing, Fail errors ) ->
            Fail (Error.InvalidTime trimmedName defaultValue :: errors)

        ( Just trimmedName, Nothing, _ ) ->
            Fail [ Error.InvalidTime trimmedName defaultValue ]

        ( Just trimmedName, Just defaultTime, Single storyID payload ) ->
            Single storyID
                { knobs = ( trimmedName, Time defaultTime ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract trimmedName knobs of
                            Just (TimeValue (Just value)) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs defaultTime
                }

        ( _, _, Label title ) ->
            Label title

        ( _, _, Todo title ) ->
            Todo title

        ( _, _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, _, Fail errors ) ->
            Fail errors


type alias File =
    File.File


files : String -> Story (List File -> a) -> Story a
files name story =
    case ( nonBlank name, story ) of
        ( Nothing, Fail errors ) ->
            Fail (Error.EmptyKnobTitle :: errors)

        ( Nothing, _ ) ->
            Fail [ Error.EmptyKnobTitle ]

        ( _, Single storyID payload ) ->
            Single storyID
                { knobs = ( name, Files ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract name knobs of
                            Just (FileValue value) ->
                                payload.view knobs value

                            _ ->
                                payload.view knobs []
                }

        ( _, Label title ) ->
            Label title

        ( _, Todo title ) ->
            Todo title

        ( _, Batch folderID stories ) ->
            Batch folderID stories

        ( _, Fail errors ) ->
            Fail errors
