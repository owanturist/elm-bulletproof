module Bulletproof.Knob exposing (int, string)

import AVL.Dict as Dict
import Internal exposing (Knob(..), KnobsState, Story(..))


extract : String -> String -> KnobsState -> Maybe Knob
extract title name state =
    state
        |> Dict.get title
        |> Maybe.andThen (Dict.get name)


int : String -> Int -> Story (Int -> a) -> Story a
int name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, KnobInt defaultValue ) :: story.knobs
        , view =
            \state ->
                case extract story.title name state.knobs of
                    Just (KnobInt value) ->
                        story.view state value

                    _ ->
                        story.view state defaultValue
        }


string : String -> String -> Story (String -> a) -> Story a
string name defaultValue (Story story) =
    Story
        { title = story.title
        , knobs = ( name, KnobString defaultValue ) :: story.knobs
        , view =
            \state ->
                case extract story.title name state.knobs of
                    Just (KnobString value) ->
                        story.view state value

                    _ ->
                        story.view state defaultValue
        }
