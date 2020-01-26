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
import Css
import Date exposing (Date, Time)
import File exposing (File)
import Html.Styled exposing (Html, div, input, label, option, styled, text, textarea)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
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


mapLimits : (a -> b) -> Limits a -> Limits b
mapLimits fn { min, max, step } =
    Limits
        (Maybe.map fn min)
        (Maybe.map fn max)
        (Maybe.map fn step)


type Choice
    = Radio
    | Select


type Value
    = BoolValue Bool
    | StringValue String
    | IntValue String
    | FloatValue String
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
            insert name (IntValue "") state

        UpdateInt name str ->
            if String.toInt str == Nothing then
                state

            else
                insert name (IntValue str) state

        UpdateFloat name "" ->
            insert name (FloatValue "") state

        UpdateFloat name str ->
            if String.toFloat str == Nothing then
                state

            else
                insert name (FloatValue str) state

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
        [ Attributes.name name
        , Attributes.checked checked
        , Events.onCheck (UpdateBool name)
        ]


viewKnobString : String -> String -> Html Msg
viewKnobString name value =
    textarea
        [ Attributes.name name
        , Attributes.value value
        , Events.onInput (UpdateString name)
        ]
        []


viewKnobNumber :
    (String -> String -> msg)
    -> String
    -> String
    -> Limits String
    -> Html msg
viewKnobNumber msg name number payload =
    input
        (Attributes.type_ "number"
            :: Attributes.name name
            :: Attributes.value number
            :: Events.onInput (msg name)
            :: List.filterMap identity
                [ Maybe.map Attributes.min payload.min
                , Maybe.map Attributes.max payload.max
                , Maybe.map Attributes.step payload.step
                ]
        )
        []


viewKnobRadio : String -> List String -> String -> Html Msg
viewKnobRadio name options current =
    Keyed.node "div"
        []
        (List.map
            (\value ->
                ( value
                , div []
                    [ label []
                        [ input
                            [ Attributes.type_ "radio"
                            , Attributes.name name
                            , Attributes.value value
                            , Attributes.checked (value == current)
                            , Events.onCheck (\_ -> UpdateString name value)
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
    Keyed.node "select"
        [ Attributes.name name
        , Events.onInput (UpdateString name)
        ]
        (List.map
            (\value ->
                ( value
                , option
                    [ Attributes.value value
                    , Attributes.selected (value == current)
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
        [ Attributes.type_ "color"
        , Attributes.name name
        , Attributes.value color
        , Events.onInput (UpdateColor name)
        ]
        []


viewKnobDate : String -> String -> Html Msg
viewKnobDate name value =
    input
        [ Attributes.type_ "date"
        , Attributes.name name
        , Attributes.value value
        , Events.onInput (UpdateDate name)
        ]
        []


viewKnobTime : String -> String -> Html Msg
viewKnobTime name value =
    input
        [ Attributes.type_ "time"
        , Attributes.name name
        , Attributes.value value
        , Events.onInput (UpdateTime name)
        ]
        []


filesDecoder : Decoder (List File)
filesDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)


viewKnobFile : String -> Html Msg
viewKnobFile name =
    input
        [ Attributes.type_ "file"
        , Attributes.multiple True
        , Attributes.name name
        , Events.on "change" (Decode.map (UpdateFiles name) filesDecoder)
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

        Int False defaultValue limits ->
            case extract name state of
                Just (IntValue value) ->
                    viewKnobNumber UpdateInt name value (mapLimits String.fromInt limits)

                _ ->
                    viewKnobNumber UpdateInt name (String.fromInt defaultValue) (mapLimits String.fromInt limits)

        Int True defaultValue limits ->
            let
                value =
                    case extract name state of
                        Just (IntValue str) ->
                            Maybe.withDefault defaultValue (String.toInt str)

                        _ ->
                            defaultValue
            in
            range (UpdateInt name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
                , value = value
                }

        Float False defaultValue limits ->
            case extract name state of
                Just (FloatValue value) ->
                    viewKnobNumber UpdateFloat name value (mapLimits String.fromFloat limits)

                _ ->
                    viewKnobNumber UpdateFloat name (String.fromFloat defaultValue) (mapLimits String.fromFloat limits)

        Float True defaultValue limits ->
            let
                value =
                    case extract name state of
                        Just (FloatValue str) ->
                            Maybe.withDefault defaultValue (String.toFloat str)

                        _ ->
                            defaultValue
            in
            range (UpdateFloat name)
                name
                String.fromFloat
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
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


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.display Css.table
        , Css.width (Css.pct 100)
        ]
        []


view : List ( String, Knob ) -> State -> Html Msg
view knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob state name knob))
        |> styledRoot
