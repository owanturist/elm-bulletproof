module Bulletproof.Knob exposing (float, int, string)

import Internal exposing (Story(..))
import Internal.Knob exposing (Knob(..), extract)


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, String defaultValue ) :: story.knobs
        , view =
            \state ->
                case extract story.title name state.knobs of
                    Just (String value) ->
                        story.view state value

                    _ ->
                        story.view state defaultValue
        }


int : String -> Int -> Story (Int -> a) -> Story a
int name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Int defaultValue ) :: story.knobs
        , view =
            \state ->
                case extract story.title name state.knobs of
                    Just (Int value) ->
                        story.view state value

                    _ ->
                        story.view state defaultValue
        }


float : String -> Float -> Story (Float -> a) -> Story a
float name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, Float defaultValue ) :: story.knobs
        , view =
            \state ->
                case extract story.title name state.knobs of
                    Just (Float value) ->
                        story.view state value

                    _ ->
                        story.view state defaultValue
        }
