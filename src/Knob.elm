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
    Dict String Value


initial : State
initial =
    Dict.empty


extract : String -> State -> Maybe Value
extract =
    Dict.get


insert : String -> Value -> State -> State
insert =
    Dict.insert



-- U P D A T E


type Msg
    = UpdateBool String Bool
    | UpdateString String String
    | UpdateInt String String
    | UpdateFloat String String
    | UpdateColor String String
    | UpdateDate String String
    | UpdateTime String String
    | UpdateFiles String (List File)


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateBool name bool ->
            insert name (BoolValue bool) state

        UpdateString name string ->
            insert name (StringValue string) state

        UpdateInt name "" ->
            insert name (IntValue Nothing) state

        UpdateInt name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just int ->
                    insert name (IntValue (Just int)) state

        UpdateFloat name "" ->
            insert name (FloatValue Nothing) state

        UpdateFloat name str ->
            case String.toFloat str of
                Nothing ->
                    state

                Just float ->
                    insert name (FloatValue (Just float)) state

        UpdateColor name "" ->
            insert name (ColorValue Nothing) state

        UpdateColor name str ->
            case Color.fromString str of
                Nothing ->
                    state

                Just color ->
                    insert name (ColorValue (Just color)) state

        UpdateDate name "" ->
            insert name (DateValue Nothing) state

        UpdateDate name str ->
            case Date.posixFromString str of
                Nothing ->
                    state

                Just date ->
                    insert name (DateValue (Just date)) state

        UpdateTime name "" ->
            insert name (TimeValue Nothing) state

        UpdateTime name str ->
            case Date.timeFromString str of
                Nothing ->
                    state

                Just time ->
                    insert name (TimeValue (Just time)) state

        UpdateFiles name files ->
            insert name (FileValue files) state



-- V I E W


viewKnobBool : String -> Bool -> Html Msg
viewKnobBool name checked =
    checkbox
        [ Html.Attributes.name name
        , Html.Attributes.checked checked
        , Html.Events.onCheck (UpdateBool name)
        ]


viewKnobString : String -> String -> Html Msg
viewKnobString name value =
    textarea
        [ Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString name)
        ]
        []


viewKnobNumber :
    (String -> String -> msg)
    -> (number -> String)
    -> String
    -> Maybe number
    -> Limits number
    -> Html msg
viewKnobNumber msg numberToString name number payload =
    input
        (Html.Attributes.type_ "number"
            :: Html.Attributes.name name
            :: Html.Attributes.value (Maybe.withDefault "" (Maybe.map numberToString number))
            :: Html.Events.onInput (msg name)
            :: List.filterMap identity
                [ Maybe.map (Html.Attributes.min << numberToString) payload.min
                , Maybe.map (Html.Attributes.max << numberToString) payload.max
                , Maybe.map (Html.Attributes.step << numberToString) payload.step
                ]
        )
        []


viewKnobRadio : String -> List String -> String -> Html Msg
viewKnobRadio name options current =
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
                            , Html.Events.onCheck (\_ -> UpdateString name value)
                            ]
                            []
                        , text value
                        ]
                    ]
                )
            )
            options
        )


viewKnobSelect : String -> List String -> String -> Html Msg
viewKnobSelect name options current =
    Html.Keyed.node "select"
        [ Html.Attributes.name name
        , Html.Events.onInput (UpdateString name)
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


viewKnobColor : String -> String -> Html Msg
viewKnobColor name color =
    input
        [ Html.Attributes.type_ "color"
        , Html.Attributes.name name
        , Html.Attributes.value color
        , Html.Events.onInput (UpdateColor name)
        ]
        []


viewKnobDate : String -> String -> Html Msg
viewKnobDate name value =
    input
        [ Html.Attributes.type_ "date"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateDate name)
        ]
        []


viewKnobTime : String -> String -> Html Msg
viewKnobTime name value =
    input
        [ Html.Attributes.type_ "time"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateTime name)
        ]
        []


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


viewKnobFile : String -> Html Msg
viewKnobFile name =
    input
        [ Html.Attributes.type_ "file"
        , Html.Attributes.multiple True
        , Html.Attributes.name name
        , Html.Events.on "change" (Decode.map (UpdateFiles name) filesDecoder)
        ]
        []


viewKnobRow : String -> Html msg -> Html msg
viewKnobRow name knob =
    div
        []
        [ text name
        , knob
        ]


viewKnob : State -> String -> Knob -> Html Msg
viewKnob state name knob =
    case knob of
        Bool defaultValue ->
            case extract name state of
                Just (BoolValue bool) ->
                    viewKnobBool name bool

                _ ->
                    viewKnobBool name defaultValue

        String defaultValue ->
            case extract name state of
                Just (StringValue string) ->
                    viewKnobString name string

                _ ->
                    viewKnobString name defaultValue

        Int False defaultValue payload ->
            case extract name state of
                Just (IntValue value) ->
                    viewKnobNumber UpdateInt String.fromInt name value payload

                _ ->
                    viewKnobNumber UpdateInt String.fromInt name (Just defaultValue) payload

        Int True defaultValue payload ->
            let
                value =
                    case extract name state of
                        Just (IntValue (Just int)) ->
                            int

                        _ ->
                            defaultValue
            in
            range (UpdateInt name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 payload.min
                , max = Maybe.withDefault 100 payload.max
                , step = Maybe.withDefault 1 payload.step
                , value = value
                }

        Float False defaultValue payload ->
            case extract name state of
                Just (FloatValue value) ->
                    viewKnobNumber UpdateFloat String.fromFloat name value payload

                _ ->
                    viewKnobNumber UpdateFloat String.fromFloat name (Just defaultValue) payload

        Float True defaultValue payload ->
            let
                value =
                    case extract name state of
                        Just (FloatValue (Just float)) ->
                            float

                        _ ->
                            defaultValue
            in
            range (UpdateFloat name)
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
            case extract name state of
                Just (StringValue string) ->
                    viewKnobRadio name options string

                _ ->
                    viewKnobRadio name options defaultValue

        Choice Select ((defaultValue :: _) as options) ->
            case extract name state of
                Just (StringValue string) ->
                    viewKnobSelect name options string

                _ ->
                    viewKnobSelect name options defaultValue

        Color Nothing ->
            viewKnobColor name ""

        Color (Just defaultValue) ->
            case extract name state of
                Just (ColorValue Nothing) ->
                    viewKnobColor name ""

                Just (ColorValue (Just color)) ->
                    viewKnobColor name color.hex

                _ ->
                    viewKnobColor name defaultValue.hex

        Date Nothing ->
            viewKnobDate name ""

        Date (Just defaultValue) ->
            case extract name state of
                Just (DateValue Nothing) ->
                    viewKnobDate name ""

                Just (DateValue (Just date)) ->
                    viewKnobDate name (Date.posixToString date)

                _ ->
                    viewKnobDate name (Date.posixToString defaultValue)

        Time Nothing ->
            viewKnobTime name ""

        Time (Just defaultValue) ->
            case extract name state of
                Just (TimeValue Nothing) ->
                    viewKnobTime name ""

                Just (TimeValue (Just time)) ->
                    viewKnobTime name (Date.timeToString time)

                _ ->
                    viewKnobTime name (Date.timeToString defaultValue)

        Files ->
            viewKnobFile name


view : List ( String, Knob ) -> State -> Html Msg
view knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob state name knob))
        |> div []
