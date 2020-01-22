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
import Path exposing (Path)
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
    Dict Path (Dict String Value)


initial : State
initial =
    Dict.emptyWith Path.compare


extract : Path -> String -> State -> Maybe Value
extract path name state =
    Maybe.andThen (Dict.get name) (Dict.get path state)


insert : Path -> String -> Value -> State -> State
insert path name value state =
    Dict.insert path
        (state
            |> Dict.get path
            |> Maybe.withDefault Dict.empty
            |> Dict.insert name value
        )
        state



-- U P D A T E


type Msg
    = UpdateBool Path String Bool
    | UpdateString Path String String
    | UpdateInt Path String String
    | UpdateFloat Path String String
    | UpdateColor Path String String
    | UpdateDate Path String String
    | UpdateTime Path String String
    | UpdateFiles Path String (List File)


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateBool path name bool ->
            insert path name (BoolValue bool) state

        UpdateString path name string ->
            insert path name (StringValue string) state

        UpdateInt path name "" ->
            insert path name (IntValue Nothing) state

        UpdateInt path name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just int ->
                    insert path name (IntValue (Just int)) state

        UpdateFloat path name "" ->
            insert path name (FloatValue Nothing) state

        UpdateFloat path name str ->
            case String.toFloat str of
                Nothing ->
                    state

                Just float ->
                    insert path name (FloatValue (Just float)) state

        UpdateColor path name "" ->
            insert path name (ColorValue Nothing) state

        UpdateColor path name str ->
            case Color.fromString str of
                Nothing ->
                    state

                Just color ->
                    insert path name (ColorValue (Just color)) state

        UpdateDate path name "" ->
            insert path name (DateValue Nothing) state

        UpdateDate path name str ->
            case Date.posixFromString str of
                Nothing ->
                    state

                Just date ->
                    insert path name (DateValue (Just date)) state

        UpdateTime path name "" ->
            insert path name (TimeValue Nothing) state

        UpdateTime path name str ->
            case Date.timeFromString str of
                Nothing ->
                    state

                Just time ->
                    insert path name (TimeValue (Just time)) state

        UpdateFiles path name files ->
            insert path name (FileValue files) state



-- V I E W


viewKnobBool : Path -> String -> Bool -> Html Msg
viewKnobBool path name checked =
    checkbox
        [ Html.Attributes.name name
        , Html.Attributes.checked checked
        , Html.Events.onCheck (UpdateBool path name)
        ]


viewKnobString : Path -> String -> String -> Html Msg
viewKnobString path name value =
    textarea
        [ Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateString path name)
        ]
        []


viewKnobNumber :
    (Path -> String -> String -> msg)
    -> (number -> String)
    -> Path
    -> String
    -> Maybe number
    -> Limits number
    -> Html msg
viewKnobNumber msg numberToString path name number payload =
    input
        (Html.Attributes.type_ "number"
            :: Html.Attributes.name name
            :: Html.Attributes.value (Maybe.withDefault "" (Maybe.map numberToString number))
            :: Html.Events.onInput (msg path name)
            :: List.filterMap identity
                [ Maybe.map (Html.Attributes.min << numberToString) payload.min
                , Maybe.map (Html.Attributes.max << numberToString) payload.max
                , Maybe.map (Html.Attributes.step << numberToString) payload.step
                ]
        )
        []


viewKnobRadio : Path -> String -> List String -> String -> Html Msg
viewKnobRadio path name options current =
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
                            , Html.Events.onCheck (\_ -> UpdateString path name value)
                            ]
                            []
                        , text value
                        ]
                    ]
                )
            )
            options
        )


viewKnobSelect : Path -> String -> List String -> String -> Html Msg
viewKnobSelect path name options current =
    Html.Keyed.node "select"
        [ Html.Attributes.name name
        , Html.Events.onInput (UpdateString path name)
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


viewKnobColor : Path -> String -> String -> Html Msg
viewKnobColor path name color =
    input
        [ Html.Attributes.type_ "color"
        , Html.Attributes.name name
        , Html.Attributes.value color
        , Html.Events.onInput (UpdateColor path name)
        ]
        []


viewKnobDate : Path -> String -> String -> Html Msg
viewKnobDate path name value =
    input
        [ Html.Attributes.type_ "date"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateDate path name)
        ]
        []


viewKnobTime : Path -> String -> String -> Html Msg
viewKnobTime path name value =
    input
        [ Html.Attributes.type_ "time"
        , Html.Attributes.name name
        , Html.Attributes.value value
        , Html.Events.onInput (UpdateTime path name)
        ]
        []


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


viewKnobFile : Path -> String -> Html Msg
viewKnobFile path name =
    input
        [ Html.Attributes.type_ "file"
        , Html.Attributes.multiple True
        , Html.Attributes.name name
        , Html.Events.on "change" (Decode.map (UpdateFiles path name) filesDecoder)
        ]
        []


viewKnobRow : String -> Html msg -> Html msg
viewKnobRow name knob =
    div
        []
        [ text name
        , knob
        ]


viewKnob : Path -> State -> String -> Knob -> Html Msg
viewKnob path state name knob =
    case knob of
        Bool defaultValue ->
            case extract path name state of
                Just (BoolValue bool) ->
                    viewKnobBool path name bool

                _ ->
                    viewKnobBool path name defaultValue

        String defaultValue ->
            case extract path name state of
                Just (StringValue string) ->
                    viewKnobString path name string

                _ ->
                    viewKnobString path name defaultValue

        Int False defaultValue payload ->
            case extract path name state of
                Just (IntValue value) ->
                    viewKnobNumber UpdateInt String.fromInt path name value payload

                _ ->
                    viewKnobNumber UpdateInt String.fromInt path name (Just defaultValue) payload

        Int True defaultValue payload ->
            let
                value =
                    case extract path name state of
                        Just (IntValue (Just int)) ->
                            int

                        _ ->
                            defaultValue
            in
            range (UpdateInt path name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 payload.min
                , max = Maybe.withDefault 100 payload.max
                , step = Maybe.withDefault 1 payload.step
                , value = value
                }

        Float False defaultValue payload ->
            case extract path name state of
                Just (FloatValue value) ->
                    viewKnobNumber UpdateFloat String.fromFloat path name value payload

                _ ->
                    viewKnobNumber UpdateFloat String.fromFloat path name (Just defaultValue) payload

        Float True defaultValue payload ->
            let
                value =
                    case extract path name state of
                        Just (FloatValue (Just float)) ->
                            float

                        _ ->
                            defaultValue
            in
            range (UpdateFloat path name)
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
            case extract path name state of
                Just (StringValue string) ->
                    viewKnobRadio path name options string

                _ ->
                    viewKnobRadio path name options defaultValue

        Choice Select ((defaultValue :: _) as options) ->
            case extract path name state of
                Just (StringValue string) ->
                    viewKnobSelect path name options string

                _ ->
                    viewKnobSelect path name options defaultValue

        Color Nothing ->
            viewKnobColor path name ""

        Color (Just defaultValue) ->
            case extract path name state of
                Just (ColorValue Nothing) ->
                    viewKnobColor path name ""

                Just (ColorValue (Just color)) ->
                    viewKnobColor path name color.hex

                _ ->
                    viewKnobColor path name defaultValue.hex

        Date Nothing ->
            viewKnobDate path name ""

        Date (Just defaultValue) ->
            case extract path name state of
                Just (DateValue Nothing) ->
                    viewKnobDate path name ""

                Just (DateValue (Just date)) ->
                    viewKnobDate path name (Date.posixToString date)

                _ ->
                    viewKnobDate path name (Date.posixToString defaultValue)

        Time Nothing ->
            viewKnobTime path name ""

        Time (Just defaultValue) ->
            case extract path name state of
                Just (TimeValue Nothing) ->
                    viewKnobTime path name ""

                Just (TimeValue (Just time)) ->
                    viewKnobTime path name (Date.timeToString time)

                _ ->
                    viewKnobTime path name (Date.timeToString defaultValue)

        Files ->
            viewKnobFile path name


view : Path -> List ( String, Knob ) -> State -> Html Msg
view path knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob path state name knob))
        |> div []
