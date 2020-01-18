module Bulletproof.Knob exposing (float, int, string)

import Internal exposing (Story(..))
import Internal.Knob exposing (Knob(..), extract)
import Json.Decode as Decode


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, String defaultValue ) :: story.knobs
        , view =
            \state ->
                state.knobs
                    |> extract Decode.string story.title name
                    |> Maybe.withDefault defaultValue
                    |> story.view state
        }


int : String -> Int -> Story (Int -> a) -> Story a
int name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Int defaultValue ) :: story.knobs
        , view =
            \state ->
                state.knobs
                    |> extract Decode.int story.title name
                    |> Maybe.withDefault defaultValue
                    |> story.view state
        }


float : String -> Float -> Story (Float -> a) -> Story a
float name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Float defaultValue ) :: story.knobs
        , view =
            \state ->
                state.knobs
                    |> extract Decode.float story.title name
                    |> Maybe.withDefault defaultValue
                    |> story.view state
        }
