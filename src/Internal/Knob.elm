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
import Html exposing (Html, div, input, label, option, text, textarea)
import Html.Attributes
import Html.Events
import Html.Keyed
import Internal.Color as Color exposing (Color)
import Internal.Date as Date exposing (Date)
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
            let
                value =
                    case extract storyID name state of
                        Just (BoolValue bool) ->
                            bool

                        _ ->
                            defaultValue
            in
            viewKnobBool storyID name value

        String defaultValue ->
            let
                value =
                    case extract storyID name state of
                        Just (StringValue string) ->
                            string

                        _ ->
                            defaultValue
            in
            viewKnobString storyID name value

        Int defaultValue ->
            let
                value =
                    case extract storyID name state of
                        Just (IntValue Nothing) ->
                            ""

                        Just (IntValue (Just int)) ->
                            String.fromInt int

                        _ ->
                            String.fromInt defaultValue
            in
            viewKnobInt storyID name value

        Float defaultValue ->
            let
                value =
                    case extract storyID name state of
                        Just (FloatValue Nothing) ->
                            ""

                        Just (FloatValue (Just float)) ->
                            String.fromFloat float

                        _ ->
                            String.fromFloat defaultValue
            in
            viewKnobFloat storyID name value

        Choice _ [] ->
            text "No Options available"

        Choice Radio (firstOption :: restOptions) ->
            let
                value =
                    case extract storyID name state of
                        Just (StringValue string) ->
                            string

                        _ ->
                            firstOption
            in
            viewKnobRadio storyID name (firstOption :: restOptions) value

        Choice Select (firstOption :: restOptions) ->
            let
                value =
                    case extract storyID name state of
                        Just (StringValue string) ->
                            string

                        _ ->
                            firstOption
            in
            viewKnobSelect storyID name (firstOption :: restOptions) value

        IntRange defaultValue limits ->
            let
                value =
                    case extract storyID name state of
                        Just (IntValue (Just int)) ->
                            int

                        _ ->
                            defaultValue
            in
            viewKnobRange (UpdateInt storyID name) String.fromInt name limits value

        FloatRange defaultValue limits ->
            let
                value =
                    case extract storyID name state of
                        Just (FloatValue (Just float)) ->
                            float

                        _ ->
                            defaultValue
            in
            viewKnobRange (UpdateFloat storyID name) String.fromFloat name limits value

        Color Nothing ->
            viewKnobColor storyID name ""

        Color (Just defaultValue) ->
            let
                value =
                    case extract storyID name state of
                        Just (ColorValue Nothing) ->
                            ""

                        Just (ColorValue (Just color)) ->
                            color.hex

                        _ ->
                            defaultValue.hex
            in
            viewKnobColor storyID name value

        Date Nothing ->
            viewKnobDate storyID name ""

        Date (Just defaultValue) ->
            let
                value =
                    case extract storyID name state of
                        Just (DateValue Nothing) ->
                            ""

                        Just (DateValue (Just date)) ->
                            Date.toString date

                        _ ->
                            Date.toString defaultValue
            in
            viewKnobDate storyID name value


view : String -> List ( String, Knob ) -> State -> Html Msg
view storyID knobs state =
    knobs
        |> List.reverse
        |> List.map (\( name, knob ) -> viewKnobRow name (viewKnob storyID state name knob))
        |> div []
