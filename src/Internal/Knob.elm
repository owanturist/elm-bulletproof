module Internal.Knob exposing (Knob(..), Msg, State, extract, initial, update, view)

import AVL.Dict as Dict exposing (Dict)
import Html exposing (Html, div, input, text)
import Html.Attributes
import Html.Events


type Knob
    = String String
    | Int Int


type alias State =
    Dict String (Dict String Knob)


initial : State
initial =
    Dict.empty


extract : String -> String -> State -> Maybe Knob
extract storyID name state =
    state
        |> Dict.get storyID
        |> Maybe.andThen (Dict.get name)


insert : String -> String -> Knob -> State -> State
insert storyID name knob state =
    Dict.insert storyID
        (state
            |> Dict.get storyID
            |> Maybe.withDefault Dict.empty
            |> Dict.insert name knob
        )
        state



-- U P D A T E


type Msg
    = UpdateInt String String String
    | UpdateString String String String


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateInt storyID name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just next ->
                    insert storyID name (Int next) state

        UpdateString storyID name next ->
            insert storyID name (String next) state



-- V I E W


viewKnobInt : String -> String -> Int -> Html Msg
viewKnobInt storyID name value =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.name name
        , Html.Attributes.value (String.fromInt value)
        , Html.Events.onInput (UpdateInt storyID name)
        ]
        []


viewKnobString : String -> String -> String -> Html Msg
viewKnobString storyID name value =
    input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString storyID name)
        ]
        []


viewKnobRow : String -> Html msg -> Html msg
viewKnobRow name knob =
    div
        []
        [ text name
        , knob
        ]


viewKnob : String -> State -> ( String, Knob ) -> Html Msg
viewKnob storyID state ( name, knob ) =
    case Maybe.withDefault knob (extract storyID name state) of
        Int value ->
            viewKnobRow name (viewKnobInt storyID name value)

        String value ->
            viewKnobRow name (viewKnobString storyID name value)


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    div [] (List.foldl ((::) << viewKnob storyID state) [] knobs)
