module Bulletproof.Knob exposing
    ( Color
    , Limits
    , bool
    , color
    , float
    , floatRange
    , int
    , intRange
    , radio
    , select
    , string
    )

import Internal exposing (Story(..))
import Internal.Color as Color
import Internal.Knob exposing (Choice(..), Knob(..), extract)
import Json.Decode as Decode exposing (Decoder)


type alias Limits number =
    Internal.Knob.Limits number


type alias Color =
    Color.Color


bool : String -> Bool -> Story (Bool -> a) -> Story a
bool name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Bool defaultValue ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    state.knobs
                        |> extract Decode.bool story.title name
                        |> Maybe.withDefault defaultValue
                        |> view state
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
                    state.knobs
                        |> extract Decode.string story.title name
                        |> Maybe.withDefault defaultValue
                        |> view state
                )
                story.view
        }


int : String -> Int -> Story (Int -> a) -> Story a
int name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Int defaultValue ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    state.knobs
                        |> extract Decode.int story.title name
                        |> Maybe.withDefault defaultValue
                        |> view state
                )
                story.view
        }


float : String -> Float -> Story (Float -> a) -> Story a
float name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Float defaultValue ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    state.knobs
                        |> extract Decode.float story.title name
                        |> Maybe.withDefault defaultValue
                        |> view state
                )
                story.view
        }


makeRange : (number -> Limits number -> Knob) -> Decoder number -> String -> number -> Limits number -> Story (number -> a) -> Story a
makeRange knob decoder name defaultValue limits (Story story) =
    Story
        { title = story.title
        , knobs = ( name, knob defaultValue limits ) :: story.knobs
        , view =
            Result.map
                (\view state ->
                    state.knobs
                        |> extract decoder story.title name
                        |> Maybe.withDefault defaultValue
                        |> view state
                )
                story.view
        }


intRange : String -> Int -> { min : Int, max : Int, step : Int } -> Story (Int -> a) -> Story a
intRange =
    makeRange IntRange Decode.int


floatRange : String -> Float -> { min : Float, max : Float, step : Float } -> Story (Float -> a) -> Story a
floatRange =
    makeRange FloatRange Decode.float


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
                                    state.knobs
                                        |> extract Decode.string story.title name
                                        |> Maybe.withDefault firstLabel
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
        , knobs = ( name, Color (Result.withDefault Color.black defaultColor) ) :: story.knobs
        , view =
            Result.map2
                (\default view state ->
                    state.knobs
                        |> extract Color.decoder story.title name
                        |> Maybe.withDefault default
                        |> view state
                )
                defaultColor
                story.view
        }
