module Internal.Knob exposing
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
import File exposing (File)
import Html exposing (Html, div, input, label, option, text, textarea)
import Html.Attributes
import Html.Events
import Html.Keyed
import Internal.Color as Color exposing (Color)
import Internal.Date as Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
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
    | Date (Maybe Time.Posix)
    | Files


type alias Limits x =
    { min : x
    , max : x
    , step : x
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
            case Date.fromString str of
                Nothing ->
                    state

                Just date ->
                    insert storyID name (DateValue (Just date)) state

        UpdateFiles storyID name files ->
            insert storyID name (FileValue files) state



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

        Int defaultValue ->
            case extract storyID name state of
                Just (IntValue Nothing) ->
                    viewKnobInt storyID name ""

                Just (IntValue (Just int)) ->
                    viewKnobInt storyID name (String.fromInt int)

                _ ->
                    viewKnobInt storyID name (String.fromInt defaultValue)

        Float defaultValue ->
            case extract storyID name state of
                Just (FloatValue Nothing) ->
                    viewKnobFloat storyID name ""

                Just (FloatValue (Just float)) ->
                    viewKnobFloat storyID name (String.fromFloat float)

                _ ->
                    viewKnobFloat storyID name (String.fromFloat defaultValue)

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

        IntRange defaultValue limits ->
            case extract storyID name state of
                Just (IntValue (Just int)) ->
                    viewKnobRange (UpdateInt storyID name) String.fromInt name limits int

                _ ->
                    viewKnobRange (UpdateInt storyID name) String.fromInt name limits defaultValue

        FloatRange defaultValue limits ->
            case extract storyID name state of
                Just (FloatValue (Just float)) ->
                    viewKnobRange (UpdateFloat storyID name) String.fromFloat name limits float

                _ ->
                    viewKnobRange (UpdateFloat storyID name) String.fromFloat name limits defaultValue

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
                    viewKnobDate storyID name (Date.toString date)

                _ ->
                    viewKnobDate storyID name (Date.toString defaultValue)

        Files ->
            viewKnobFile storyID name


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob storyID state name knob))
        |> div []
