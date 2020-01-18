module Internal.Knob exposing (Knob(..), Msg, State, extract, initial, update, view)

import AVL.Dict as Dict exposing (Dict)
import Html exposing (Html, div, input, text, textarea)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode as Encode exposing (Value, encode)


type Knob
    = String String
    | Int Int
    | Float Float


type alias State =
    Dict String (Dict String String)


initial : State
initial =
    Dict.empty


extract : Decoder value -> String -> String -> State -> Maybe value
extract decoder storyID name state =
    state
        |> Dict.get storyID
        |> Maybe.andThen (Dict.get name)
        |> Maybe.andThen (Result.toMaybe << decodeString decoder)


insert : (value -> Value) -> String -> String -> value -> State -> State
insert encoder storyID name value state =
    Dict.insert storyID
        (state
            |> Dict.get storyID
            |> Maybe.withDefault Dict.empty
            |> Dict.insert name (encode 0 (encoder value))
        )
        state



-- U P D A T E


type Msg
    = UpdateString String String String
    | UpdateInt String String String
    | UpdateFloat String String String


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateString storyID name next ->
            insert Encode.string storyID name next state

        UpdateInt storyID name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just next ->
                    insert Encode.int storyID name next state

        UpdateFloat storyID name str ->
            case String.toFloat str of
                Nothing ->
                    state

                Just next ->
                    insert Encode.float storyID name next state



-- V I E W


viewKnobString : String -> String -> String -> Html Msg
viewKnobString storyID name value =
    textarea
        [ Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString storyID name)
        ]
        []


viewKnobInt : String -> String -> Int -> Html Msg
viewKnobInt storyID name value =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.name name
        , Html.Attributes.value (String.fromInt value)
        , Html.Events.onInput (UpdateInt storyID name)
        ]
        []


viewKnobFloat : String -> String -> Float -> Html Msg
viewKnobFloat storyID name value =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.name name
        , Html.Attributes.value (String.fromFloat value)
        , Html.Events.onInput (UpdateFloat storyID name)
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
    case knob of
        String initialValue ->
            extract Decode.string storyID name state
                |> Maybe.withDefault initialValue
                |> viewKnobString storyID name
                |> viewKnobRow name

        Int initialValue ->
            extract Decode.int storyID name state
                |> Maybe.withDefault initialValue
                |> viewKnobInt storyID name
                |> viewKnobRow name

        Float initialValue ->
            extract Decode.float storyID name state
                |> Maybe.withDefault initialValue
                |> viewKnobFloat storyID name
                |> viewKnobRow name


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    div [] (List.foldl ((::) << viewKnob storyID state) [] knobs)
