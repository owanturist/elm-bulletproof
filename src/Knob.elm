module Knob exposing
    ( Choice(..)
    , Knob(..)
    , Limits
    , Msg
    , State
    , Value(..)
    , extract
    , initial
    , update
    , view
    )

import AVL.Dict as Dict exposing (Dict)
import Checkbox exposing (checkbox)
import Color exposing (Color)
import Date exposing (Date, Time)
import File exposing (File)
import Html exposing (Html, div, input, label, option, text, textarea)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)
import Range exposing (range)
import Time


type Knob
    = Bool Bool
    | String String
    | Int Bool Int (Limits Int)
    | Float Bool Float (Limits Float)
    | Choice Choice (List String)
    | Color (Maybe Color)
    | Date (Maybe Time.Posix)
    | Time (Maybe Time)
    | Files


type alias Limits x =
    { min : Maybe x
    , max : Maybe x
    , step : Maybe x
    }


type Choice
    = Radio
    | Select


type Value
    = BoolValue Bool
    | StringValue String
    | IntValue (Maybe Int)
    | FloatValue (Maybe Float)
    | ColorValue (Maybe Color)
    | DateValue (Maybe Time.Posix)
    | TimeValue (Maybe Time)
    | FileValue (List File)


type alias State =
    Dict String (Dict String Value)


initial : State
initial =
    Dict.empty


extract : String -> String -> State -> Maybe Value
extract storyID name state =
    Maybe.andThen (Dict.get name) (Dict.get storyID state)


insert : String -> String -> Value -> State -> State
insert storyID name value state =
    Dict.insert storyID
        (state
            |> Dict.get storyID
            |> Maybe.withDefault Dict.empty
            |> Dict.insert name value
        )
        state



-- U P D A T E


type Msg
    = UpdateBool String String Bool
    | UpdateString String String String
    | UpdateInt String String String
    | UpdateFloat String String String
    | UpdateColor String String String
    | UpdateDate String String String
    | UpdateTime String String String
    | UpdateFiles String String (List File)


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateBool storyID name bool ->
            insert storyID name (BoolValue bool) state

        UpdateString storyID name string ->
            insert storyID name (StringValue string) state

        UpdateInt storyID name "" ->
            insert storyID name (IntValue Nothing) state

        UpdateInt storyID name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just int ->
                    insert storyID name (IntValue (Just int)) state

        UpdateFloat storyID name "" ->
            insert storyID name (FloatValue Nothing) state

        UpdateFloat storyID name str ->
            case String.toFloat str of
                Nothing ->
                    state

                Just float ->
                    insert storyID name (FloatValue (Just float)) state

        UpdateColor storyID name "" ->
            insert storyID name (ColorValue Nothing) state

        UpdateColor storyID name str ->
            case Color.fromString str of
                Nothing ->
                    state

                Just color ->
                    insert storyID name (ColorValue (Just color)) state

        UpdateDate storyID name "" ->
            insert storyID name (DateValue Nothing) state

        UpdateDate storyID name str ->
            case Date.posixFromString str of
                Nothing ->
                    state

                Just date ->
                    insert storyID name (DateValue (Just date)) state

        UpdateTime storyID name "" ->
            insert storyID name (TimeValue Nothing) state

        UpdateTime storyID name str ->
            case Date.timeFromString str of
                Nothing ->
                    state

                Just time ->
                    insert storyID name (TimeValue (Just time)) state

        UpdateFiles storyID name files ->
            insert storyID name (FileValue files) state



-- V I E W


viewKnobBool : String -> String -> Bool -> Html Msg
viewKnobBool storyID name checked =
    checkbox
        [ Html.Attributes.name name
        , Html.Attributes.checked checked
        , Html.Events.onCheck (UpdateBool storyID name)
        ]


viewKnobString : String -> String -> String -> Html Msg
viewKnobString storyID name value =
    textarea
        [ Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString storyID name)
        ]
        []


viewKnobNumber :
    (String -> String -> String -> msg)
    -> (number -> String)
    -> String
    -> String
    -> Maybe number
    -> Limits number
    -> Html msg
viewKnobNumber msg numberToString storyID name number payload =
    input
        (Html.Attributes.type_ "number"
            :: Html.Attributes.name name
            :: Html.Attributes.value (Maybe.withDefault "" (Maybe.map numberToString number))
            :: Html.Events.onInput (msg storyID name)
            :: List.filterMap identity
                [ Maybe.map (Html.Attributes.min << numberToString) payload.min
                , Maybe.map (Html.Attributes.max << numberToString) payload.max
                , Maybe.map (Html.Attributes.step << numberToString) payload.step
                ]
        )
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


viewKnobColor : String -> String -> String -> Html Msg
viewKnobColor storyID name color =
    input
        [ Html.Attributes.type_ "color"
        , Html.Attributes.name name
        , Html.Attributes.value color
        , Html.Events.onInput (UpdateColor storyID name)
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


viewKnobTime : String -> String -> String -> Html Msg
viewKnobTime storyID name value =
    input
        [ Html.Attributes.type_ "time"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateTime storyID name)
        ]
        []


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


viewKnobFile : String -> String -> Html Msg
viewKnobFile storyID name =
    input
        [ Html.Attributes.type_ "file"
        , Html.Attributes.multiple True
        , Html.Attributes.name name
        , Html.Events.on "change" (Decode.map (UpdateFiles storyID name) filesDecoder)
        ]
        []


viewKnobRow : String -> Html msg -> Html msg
viewKnobRow name knob =
    div
        []
        [ text name
        , knob
        ]


viewKnob : String -> State -> String -> Knob -> Html Msg
viewKnob storyID state name knob =
    case knob of
        Bool defaultValue ->
            case extract storyID name state of
                Just (BoolValue bool) ->
                    viewKnobBool storyID name bool

                _ ->
                    viewKnobBool storyID name defaultValue

        String defaultValue ->
            case extract storyID name state of
                Just (StringValue string) ->
                    viewKnobString storyID name string

                _ ->
                    viewKnobString storyID name defaultValue

        Int False defaultValue payload ->
            case extract storyID name state of
                Just (IntValue value) ->
                    viewKnobNumber UpdateInt String.fromInt storyID name value payload

                _ ->
                    viewKnobNumber UpdateInt String.fromInt storyID name (Just defaultValue) payload

        Int True defaultValue payload ->
            let
                value =
                    case extract storyID name state of
                        Just (IntValue (Just int)) ->
                            int

                        _ ->
                            defaultValue
            in
            range (UpdateInt storyID name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 payload.min
                , max = Maybe.withDefault 100 payload.max
                , step = Maybe.withDefault 1 payload.step
                , value = value
                }

        Float False defaultValue payload ->
            case extract storyID name state of
                Just (FloatValue value) ->
                    viewKnobNumber UpdateFloat String.fromFloat storyID name value payload

                _ ->
                    viewKnobNumber UpdateFloat String.fromFloat storyID name (Just defaultValue) payload

        Float True defaultValue payload ->
            let
                value =
                    case extract storyID name state of
                        Just (FloatValue (Just float)) ->
                            float

                        _ ->
                            defaultValue
            in
            range (UpdateFloat storyID name)
                name
                String.fromFloat
                { min = Maybe.withDefault 0 payload.min
                , max = Maybe.withDefault 100 payload.max
                , step = Maybe.withDefault 1 payload.step
                , value = value
                }

        Choice _ [] ->
            text "No Options available"

        Choice Radio ((defaultValue :: _) as options) ->
            case extract storyID name state of
                Just (StringValue string) ->
                    viewKnobRadio storyID name options string

                _ ->
                    viewKnobRadio storyID name options defaultValue

        Choice Select ((defaultValue :: _) as options) ->
            case extract storyID name state of
                Just (StringValue string) ->
                    viewKnobSelect storyID name options string

                _ ->
                    viewKnobSelect storyID name options defaultValue

        Color Nothing ->
            viewKnobColor storyID name ""

        Color (Just defaultValue) ->
            case extract storyID name state of
                Just (ColorValue Nothing) ->
                    viewKnobColor storyID name ""

                Just (ColorValue (Just color)) ->
                    viewKnobColor storyID name color.hex

                _ ->
                    viewKnobColor storyID name defaultValue.hex

        Date Nothing ->
            viewKnobDate storyID name ""

        Date (Just defaultValue) ->
            case extract storyID name state of
                Just (DateValue Nothing) ->
                    viewKnobDate storyID name ""

                Just (DateValue (Just date)) ->
                    viewKnobDate storyID name (Date.posixToString date)

                _ ->
                    viewKnobDate storyID name (Date.posixToString defaultValue)

        Time Nothing ->
            viewKnobTime storyID name ""

        Time (Just defaultValue) ->
            case extract storyID name state of
                Just (TimeValue Nothing) ->
                    viewKnobTime storyID name ""

                Just (TimeValue (Just time)) ->
                    viewKnobTime storyID name (Date.timeToString time)

                _ ->
                    viewKnobTime storyID name (Date.timeToString defaultValue)

        Files ->
            viewKnobFile storyID name


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob storyID state name knob))
        |> div []
