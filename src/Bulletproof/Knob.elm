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
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Bool defaultValue ) :: payload.knobs
                , view =
                    Result.map
                        (\view state ->
                            case extract payload.title name state.knobs of
                                Just (BoolValue value) ->
                                    view state value

                                _ ->
                                    view state defaultValue
                        )
                        payload.view
                }

        _ ->
            Empty


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue story =
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, String defaultValue ) :: payload.knobs
                , view =
                    Result.map
                        (\view state ->
                            case extract payload.title name state.knobs of
                                Just (StringValue value) ->
                                    view state value

                                _ ->
                                    view state defaultValue
                        )
                        payload.view
                }

        _ ->
            Empty


type Property num
    = Range Bool
    | Min num
    | Max num
    | Step num


range : Bool -> Property num
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
        Range x ->
            ( x, limits )

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
    let
        ( range_, limits ) =
            propertiesToNumberPayload properties
    in
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Int range_ defaultValue limits ) :: payload.knobs
                , view =
                    Result.map
                        (\view state ->
                            case extract payload.title name state.knobs of
                                Just (IntValue (Just value)) ->
                                    view state value

                                _ ->
                                    view state defaultValue
                        )
                        payload.view
                }

        _ ->
            Empty


float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultValue properties story =
    let
        ( range_, limits ) =
            propertiesToNumberPayload properties
    in
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Float range_ defaultValue limits ) :: payload.knobs
                , view =
                    Result.map
                        (\view state ->
                            case extract payload.title name state.knobs of
                                Just (FloatValue (Just value)) ->
                                    view state value

                                _ ->
                                    view state defaultValue
                        )
                        payload.view
                }

        _ ->
            Empty


makeChoice : Choice -> String -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice choiceName name options story =
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Choice choice (List.map Tuple.first options) ) :: payload.knobs
                , view =
                    case List.head options of
                        Nothing ->
                            Err (choiceName ++ " Knob '" ++ name ++ "' expects at least one option")

                        Just ( firstLabel, firstValue ) ->
                            Result.map
                                (\view state ->
                                    let
                                        selected =
                                            case extract payload.title name state.knobs of
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
                                        |> view state
                                )
                                payload.view
                }

        _ ->
            Empty


radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio =
    makeChoice Radio "Radio"


select : String -> List ( String, option ) -> Story (option -> a) -> Story a
select =
    makeChoice Select "Select"


color : String -> String -> Story (Color -> a) -> Story a
color name defaultValue story =
    let
        defaultColor =
            Color.fromString defaultValue
    in
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Color defaultColor ) :: payload.knobs
                , view =
                    Result.map2
                        (\default view state ->
                            case extract payload.title name state.knobs of
                                Just (ColorValue (Just value)) ->
                                    view state value

                                _ ->
                                    view state default
                        )
                        (Result.fromMaybe ("Color in '" ++ name ++ "' is invalid.") defaultColor)
                        payload.view
                }

        _ ->
            Empty


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue story =
    let
        defaultDate =
            Date.parseStringToPosix defaultValue
    in
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Date defaultDate ) :: payload.knobs
                , view =
                    Result.map2
                        (\default view state ->
                            case extract payload.title name state.knobs of
                                Just (DateValue (Just value)) ->
                                    view state (Date.dateFromPosix value)

                                _ ->
                                    view state (Date.dateFromPosix default)
                        )
                        (Result.fromMaybe ("Date in '" ++ name ++ "' is invalid.") defaultDate)
                        payload.view
                }

        _ ->
            Empty


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue story =
    let
        defaultTime =
            Date.timeFromString defaultValue
    in
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Time defaultTime ) :: payload.knobs
                , view =
                    Result.map2
                        (\default view state ->
                            case extract payload.title name state.knobs of
                                Just (TimeValue (Just value)) ->
                                    view state value

                                _ ->
                                    view state default
                        )
                        (Result.fromMaybe ("Time in '" ++ name ++ "' is invalid.") defaultTime)
                        payload.view
                }

        _ ->
            Empty


files : String -> Story (List File -> a) -> Story a
files name story =
    case story of
        Story payload ->
            Story
                { title = payload.title
                , knobs = ( name, Files ) :: payload.knobs
                , view =
                    Result.map
                        (\view state ->
                            case extract payload.title name state.knobs of
                                Just (FileValue value) ->
                                    view state value

                                _ ->
                                    view state []
                        )
                        payload.view
                }

        _ ->
            Empty
