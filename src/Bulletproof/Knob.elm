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
import Internal exposing (Story(..))
import Knob exposing (Choice(..), Knob(..), Limits, Value(..), extract)


type alias File =
    File.File


type alias Color =
    Color.Color


type alias Date =
    Date.Date


type alias Time =
    Date.Time


bool : String -> Bool -> Story (Bool -> a) -> Story a
bool name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Bool defaultValue ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    case extract story.title name state.knobs of
                        Just (BoolValue value) ->
                            view state value

                        _ ->
                            view state defaultValue
                )
                story.view
        }


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, String defaultValue ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    case extract story.title name state.knobs of
                        Just (StringValue value) ->
                            view state value

                        _ ->
                            view state defaultValue
                )
                story.view
        }


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
int name defaultValue properties (Story story) =
    let
        ( range_, limits ) =
            propertiesToNumberPayload properties
    in
    Story
        { title = story.title
        , knobs = ( name, Int range_ defaultValue limits ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    case extract story.title name state.knobs of
                        Just (IntValue (Just value)) ->
                            view state value

                        _ ->
                            view state defaultValue
                )
                story.view
        }


float : String -> Float -> List (Property Float) -> Story (Float -> a) -> Story a
float name defaultValue properties (Story story) =
    let
        ( range_, limits ) =
            propertiesToNumberPayload properties
    in
    Story
        { title = story.title
        , knobs = ( name, Float range_ defaultValue limits ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    case extract story.title name state.knobs of
                        Just (FloatValue (Just value)) ->
                            view state value

                        _ ->
                            view state defaultValue
                )
                story.view
        }


makeChoice : Choice -> String -> String -> List ( String, option ) -> Story (option -> a) -> Story a
makeChoice choice choiceName name options (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Choice choice (List.map Tuple.first options) ) :: story.knobs
        , view =
            case List.head options of
                Nothing ->
                    Err (choiceName ++ " Knob '" ++ name ++ "' expects at least one option")

                Just ( firstLabel, firstValue ) ->
                    Result.map
                        (\view state ->
                            let
                                selected =
                                    case extract story.title name state.knobs of
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
                        story.view
        }


radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio =
    makeChoice Radio "Radio"


select : String -> List ( String, option ) -> Story (option -> a) -> Story a
select =
    makeChoice Select "Select"


color : String -> String -> Story (Color -> a) -> Story a
color name defaultValue (Story story) =
    let
        defaultColor =
            Color.fromString defaultValue
    in
    Story
        { title = story.title
        , knobs = ( name, Color defaultColor ) :: story.knobs
        , view =
            Result.map2
                (\default view state ->
                    case extract story.title name state.knobs of
                        Just (ColorValue (Just value)) ->
                            view state value

                        _ ->
                            view state default
                )
                (Result.fromMaybe ("Color in '" ++ name ++ "' is invalid.") defaultColor)
                story.view
        }


date : String -> String -> Story (Date -> a) -> Story a
date name defaultValue (Story story) =
    let
        defaultDate =
            Date.parseStringToPosix defaultValue
    in
    Story
        { title = story.title
        , knobs = ( name, Date defaultDate ) :: story.knobs
        , view =
            Result.map2
                (\default view state ->
                    case extract story.title name state.knobs of
                        Just (DateValue (Just value)) ->
                            view state (Date.dateFromPosix value)

                        _ ->
                            view state (Date.dateFromPosix default)
                )
                (Result.fromMaybe ("Date in '" ++ name ++ "' is invalid.") defaultDate)
                story.view
        }


time : String -> String -> Story (Time -> a) -> Story a
time name defaultValue (Story story) =
    let
        defaultTime =
            Date.timeFromString defaultValue
    in
    Story
        { title = story.title
        , knobs = ( name, Time defaultTime ) :: story.knobs
        , view =
            Result.map2
                (\default view state ->
                    case extract story.title name state.knobs of
                        Just (TimeValue (Just value)) ->
                            view state value

                        _ ->
                            view state default
                )
                (Result.fromMaybe ("Time in '" ++ name ++ "' is invalid.") defaultTime)
                story.view
        }


files : String -> Story (List File -> a) -> Story a
files name (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Files ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    case extract story.title name state.knobs of
                        Just (FileValue value) ->
                            view state value

                        _ ->
                            view state []
                )
                story.view
        }
