module Internal.Knob exposing
    ( Choice(..)
    , Knob(..)
    , Limits
    , Msg
    , State
    , extract
    , initial
    , update
    , view
    )

import AVL.Dict as Dict exposing (Dict)
import Html exposing (Html, div, input, label, option, text, textarea)
import Html.Attributes
import Html.Events
import Html.Keyed
import Internal.Color as Color exposing (Color)
import Internal.Date as Date
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode as Encode exposing (Value, encode)
import Time


type Knob
    = Bool Bool
    | String String
    | Int Int
    | Float Float
    | Choice Choice (List String)
    | IntRange Int (Limits Int)
    | FloatRange Float (Limits Float)
    | Color (Maybe Color)
    | Date (Maybe Int)


type alias Limits x =
    { min : x
    , max : x
    , step : x
    }


type Choice
    = Radio
    | Select


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
    = UpdateBool String String Bool
    | UpdateString String String String
    | UpdateInt String String String
    | UpdateFloat String String String
    | UpdateDate String String String


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateBool storyID name next ->
            insert Encode.bool storyID name next state

        UpdateString storyID name next ->
            insert Encode.string storyID name next state

        UpdateInt storyID name "" ->
            insert Encode.string storyID name "" state

        UpdateInt storyID name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just next ->
                    insert Encode.int storyID name next state

        UpdateFloat storyID name "" ->
            insert Encode.string storyID name "" state

        UpdateFloat storyID name str ->
            case String.toFloat str of
                Nothing ->
                    state

                Just next ->
                    insert Encode.float storyID name next state

        UpdateDate storyID name str ->
            case Date.fromString str of
                Nothing ->
                    state

                Just date ->
                    insert Encode.int storyID name (Time.posixToMillis date.posix) state



-- V I E W


viewKnobBool : String -> String -> Bool -> Html Msg
viewKnobBool storyID name checked =
    input
        [ Html.Attributes.type_ "checkbox"
        , Html.Attributes.name name
        , Html.Attributes.checked checked
        , Html.Events.onCheck (UpdateBool storyID name)
        ]
        []


viewKnobString : String -> String -> String -> Html Msg
viewKnobString storyID name value =
    textarea
        [ Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString storyID name)
        ]
        []


viewKnobInt : String -> String -> String -> Html Msg
viewKnobInt storyID name value =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateInt storyID name)
        ]
        []


viewKnobFloat : String -> String -> String -> Html Msg
viewKnobFloat storyID name value =
    input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateFloat storyID name)
        ]
        []


viewKnobRadio : String -> String -> List String -> String -> Html Msg
viewKnobRadio storyID name options current =
    Html.Keyed.node "div"
        []
        (List.map
            (\value ->
                ( value
                , div []
                    [ label []
                        [ input
                            [ Html.Attributes.type_ "radio"
                            , Html.Attributes.name name
                            , Html.Attributes.value value
                            , Html.Attributes.checked (value == current)
                            , Html.Events.onCheck (\_ -> UpdateString storyID name value)
                            ]
                            []
                        , text value
                        ]
                    ]
                )
            )
            options
        )


viewKnobSelect : String -> String -> List String -> String -> Html Msg
viewKnobSelect storyID name options current =
    Html.Keyed.node "select"
        [ Html.Attributes.name name
        , Html.Events.onInput (UpdateString storyID name)
        ]
        (List.map
            (\value ->
                ( value
                , option
                    [ Html.Attributes.value value
                    , Html.Attributes.selected (value == current)
                    ]
                    [ text value
                    ]
                )
            )
            options
        )


viewKnobRange : (String -> msg) -> (number -> String) -> String -> Limits number -> number -> Html msg
viewKnobRange msg numberToString name limits number =
    input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.name name
        , Html.Attributes.min (numberToString limits.min)
        , Html.Attributes.max (numberToString limits.max)
        , Html.Attributes.step (numberToString limits.step)
        , Html.Attributes.value (numberToString number)
        , Html.Events.onInput msg
        ]
        []


viewKnobColor : String -> String -> String -> Html Msg
viewKnobColor storyID name color =
    input
        [ Html.Attributes.type_ "color"
        , Html.Attributes.name name
        , Html.Attributes.value color
        , Html.Events.onInput (UpdateString storyID name)
        ]
        []


viewKnobDate : String -> String -> String -> Html Msg
viewKnobDate storyID name value =
    input
        [ Html.Attributes.type_ "date"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateDate storyID name)
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
        Bool defaultValue ->
            extract Decode.bool storyID name state
                |> Maybe.withDefault defaultValue
                |> viewKnobBool storyID name
                |> viewKnobRow name

        String defaultValue ->
            extract Decode.string storyID name state
                |> Maybe.withDefault defaultValue
                |> viewKnobString storyID name
                |> viewKnobRow name

        Int defaultValue ->
            let
                value =
                    case extract Decode.int storyID name state of
                        Nothing ->
                            Maybe.withDefault
                                (String.fromInt defaultValue)
                                (extract Decode.string storyID name state)

                        Just int ->
                            String.fromInt int
            in
            viewKnobRow name (viewKnobInt storyID name value)

        Float defaultValue ->
            let
                value =
                    case extract Decode.float storyID name state of
                        Nothing ->
                            Maybe.withDefault
                                (String.fromFloat defaultValue)
                                (extract Decode.string storyID name state)

                        Just float ->
                            String.fromFloat float
            in
            viewKnobRow name (viewKnobFloat storyID name value)

        Choice _ [] ->
            viewKnobRow name (text "No Options available")

        Choice Radio (firstOption :: restOptions) ->
            extract Decode.string storyID name state
                |> Maybe.withDefault firstOption
                |> viewKnobRadio storyID name (firstOption :: restOptions)
                |> viewKnobRow name

        Choice Select (firstOption :: restOptions) ->
            extract Decode.string storyID name state
                |> Maybe.withDefault firstOption
                |> viewKnobSelect storyID name (firstOption :: restOptions)
                |> viewKnobRow name

        IntRange defaultValue limits ->
            extract Decode.int storyID name state
                |> Maybe.withDefault defaultValue
                |> viewKnobRange (UpdateInt storyID name) String.fromInt name limits
                |> viewKnobRow name

        FloatRange defaultValue limits ->
            extract Decode.float storyID name state
                |> Maybe.withDefault defaultValue
                |> viewKnobRange (UpdateFloat storyID name) String.fromFloat name limits
                |> viewKnobRow name

        Color Nothing ->
            viewKnobRow name (viewKnobColor storyID name "")

        Color (Just defaultValue) ->
            extract Color.decoder storyID name state
                |> Maybe.withDefault defaultValue
                |> .hex
                |> viewKnobColor storyID name
                |> viewKnobRow name

        Date Nothing ->
            viewKnobRow name (viewKnobDate storyID name "")

        Date (Just defaultValue) ->
            extract Decode.int storyID name state
                |> Maybe.withDefault defaultValue
                |> Date.fromInt
                |> Date.toString
                |> viewKnobDate storyID name
                |> viewKnobRow name


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    div [] (List.foldl ((::) << viewKnob storyID state) [] knobs)
