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
import File
import Knob exposing (Choice(..), Knob(..), Limits, Value(..), extract)
import Story exposing (Story(..))


type alias File =
    File.File


type alias Color =
    Color.Color


type alias Date =
    Date.Date


type alias Time =
    Date.Time


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
propertyToNumberPayload property ( range_, limits ) =
    case property of
        Range ->
            ( True, limits )

        Min num ->
            ( range_, { limits | min = Just num } )

        Max num ->
            ( range_, { limits | max = Just num } )

        Step num ->
            ( range_, { limits | step = Just num } )


propertiesToNumberPayload : List (Property number) -> ( Bool, Limits number )
propertiesToNumberPayload =
    List.foldl
        propertyToNumberPayload
        ( False, Limits Nothing Nothing Nothing )


int : String -> Int -> List (Property Int) -> Story (Int -> a) -> Story a
int name defaultValue properties story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            let
                ( range_, limits ) =
                    propertiesToNumberPayload properties
            in
            Single storyID
                { knobs = ( name, Int range_ defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract name knobs of
                            Just (IntValue str) ->
                                payload.view knobs (Maybe.withDefault defaultValue (String.toInt str))

                            _ ->
                                payload.view knobs defaultValue
                }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultValue properties story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            let
                ( range_, limits ) =
                    propertiesToNumberPayload properties
            in
            Single storyID
                { knobs = ( name, Float range_ defaultValue limits ) :: payload.knobs
                , view =
                    \knobs ->
                        case extract name knobs of
                            Just (FloatValue str) ->
                                payload.view knobs (Maybe.withDefault defaultValue (String.toFloat str))

                            _ ->
                                payload.view knobs defaultValue
                }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


makeChoice : Choice -> String -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice choiceName name options story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            case List.head options of
                Nothing ->
                    Fail [ choiceName ++ " Knob '" ++ name ++ "' expects at least one option" ]

                Just ( firstLabel, firstValue ) ->
                    Single storyID
                        { knobs = ( name, Choice choice firstLabel (List.map Tuple.first options) ) :: payload.knobs
                        , view =
                            \knobs ->
                                let
                                    selected =
                                        case extract name knobs of
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

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio =
    makeChoice Radio "Radio"


select : String -> List ( String, option ) -> Story (option -> a) -> Story a
select =
    makeChoice Select "Select"


color : String -> String -> Story (Color -> a) -> Story a
color name defaultValue story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            case Color.fromString defaultValue of
                Nothing ->
                    Fail [ "Color `" ++ defaultValue ++ "` is invalid." ]

                Just defaultColor ->
                    Single storyID
                        { knobs = ( name, Color defaultColor ) :: payload.knobs
                        , view =
                            \knobs ->
                                case extract name knobs of
                                    Just (ColorValue (Just value)) ->
                                        payload.view knobs value

                                    _ ->
                                        payload.view knobs defaultColor
                        }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            case Date.parseStringToPosix defaultValue of
                Nothing ->
                    Fail [ "Date `" ++ defaultValue ++ "` is invalid" ]

                Just defaultDate ->
                    Single storyID
                        { knobs = ( name, Date defaultDate ) :: payload.knobs
                        , view =
                            \knobs ->
                                case extract name knobs of
                                    Just (DateValue (Just value)) ->
                                        payload.view knobs (Date.dateFromPosix value)

                                    _ ->
                                        payload.view knobs (Date.dateFromPosix defaultDate)
                        }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
            case Date.timeFromString defaultValue of
                Nothing ->
                    Fail [ "Time `" ++ defaultValue ++ "` is invalid" ]

                Just defaultTime ->
                    Single storyID
                        { knobs = ( name, Time defaultTime ) :: payload.knobs
                        , view =
                            \knobs ->
                                case extract name knobs of
                                    Just (TimeValue (Just value)) ->
                                        payload.view knobs value

                                    _ ->
                                        payload.view knobs defaultTime
                        }

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors


files : String -> Story (List File -> a) -> Story a
files name story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single storyID payload ->
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

        Batch folderID stories ->
            Batch folderID stories

        Fail errors ->
            Fail errors
