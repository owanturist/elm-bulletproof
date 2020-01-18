module Bulletproof.Knob exposing (float, int, radio, string)

import Internal exposing (Story(..))
import Internal.Knob exposing (Knob(..), extract)
import Json.Decode as Decode


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


radio : String -> List ( String, option ) -> Story (option -> a) -> Story a
radio name options (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Radio (List.map Tuple.first options) ) :: story.knobs
        , view =
            case List.head options of
                Nothing ->
                    Err ("Radio Knob '" ++ name ++ "' expects at least one option")

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
