module Knob exposing
    ( Knob(..)
    , KnobValue(..)
    , Limits
    , Msg
    , Payload
    , State
    , applyBool
    , applyChoice
    , applyColor
    , applyDate
    , applyFiles
    , applyFloat
    , applyInt
    , applyString
    , applyTime
    , applyViewport
    , css
    , initial
    , update
    , view
    )

import Color exposing (Color)
import Date exposing (Date, Time)
import Dict exposing (Dict)
import File exposing (File)
import Html exposing (Html, div, input, label, option, td, text, textarea, tr)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import List
import Palette
import Range exposing (range)
import String
import Style
import TextCode exposing (textCode)
import Utils exposing (Viewport, px)


type Knob
    = Bool Bool
    | String String
    | Int Bool Int (Limits Int)
    | Float Bool Float (Limits Float)
    | Radio (List String)
    | Select (List String)
    | Color String
    | Date String
    | Time String
    | Files
    | StoryViewport


type alias Limits x =
    { min : Maybe x
    , max : Maybe x
    , step : Maybe x
    }


applyLimits : Limits number -> number -> number
applyLimits limits num =
    case ( limits.min, limits.max ) of
        ( Nothing, Nothing ) ->
            num

        ( Nothing, Just limitMax ) ->
            min num limitMax

        ( Just limitMin, Nothing ) ->
            max num limitMin

        ( Just limitMin, Just limitMax ) ->
            clamp limitMin limitMax num



-- K N O B   V A L U E


type KnobValue
    = BoolValue Bool
    | StringValue String
    | IntValue (Maybe Int)
    | FloatValue (Maybe Float)
    | ColorValue (Maybe Color)
    | DateValue (Maybe Date)
    | TimeValue (Maybe Time)
    | FilesValue (List File)


type alias Payload =
    { state : State
    , viewport : Viewport
    }


type alias StoryView value view =
    Payload -> Maybe (value -> view)


getBool : String -> Bool -> State -> Bool
getBool name defaultBool state =
    case Dict.get name state of
        Just (BoolValue bool) ->
            bool

        _ ->
            defaultBool


applyBool : String -> Bool -> StoryView Bool view -> Payload -> Maybe view
applyBool name defaultBool storyView payload =
    Maybe.map
        ((|>) (getBool name defaultBool payload.state))
        (storyView payload)


getString : String -> String -> State -> String
getString name defaultString state =
    case Dict.get name state of
        Just (StringValue string) ->
            string

        _ ->
            defaultString


applyString : String -> String -> StoryView String view -> Payload -> Maybe view
applyString name defaultString storyView payload =
    Maybe.map
        ((|>) (getString name defaultString payload.state))
        (storyView payload)


getInt : String -> Int -> State -> Int
getInt name defaultInt state =
    case Dict.get name state of
        Just (IntValue int) ->
            Maybe.withDefault defaultInt int

        _ ->
            defaultInt


applyInt : String -> Int -> StoryView Int view -> Payload -> Maybe view
applyInt name defaultInt storyView payload =
    Maybe.map
        ((|>) (getInt name defaultInt payload.state))
        (storyView payload)


getFloat : String -> Float -> State -> Float
getFloat name defaultFloat state =
    case Dict.get name state of
        Just (FloatValue float) ->
            Maybe.withDefault defaultFloat float

        _ ->
            defaultFloat


applyFloat : String -> Float -> StoryView Float view -> Payload -> Maybe view
applyFloat name defaultFloat storyView payload =
    Maybe.map
        ((|>) (getFloat name defaultFloat payload.state))
        (storyView payload)


getChoice : String -> Maybe option -> Dict String option -> State -> Maybe option
getChoice name defaultOption optionsDict state =
    case Dict.get name state of
        Just (StringValue key) ->
            case Dict.get key optionsDict of
                Nothing ->
                    defaultOption

                just ->
                    just

        _ ->
            defaultOption


applyChoice : String -> Maybe option -> Dict String option -> StoryView option view -> Payload -> Maybe view
applyChoice name defaultOption optionsDict storyView payload =
    Maybe.map2
        (|>)
        (getChoice name defaultOption optionsDict payload.state)
        (storyView payload)


getColor : String -> String -> State -> Maybe Color
getColor name defaultColor state =
    case Dict.get name state of
        Just (ColorValue (Just color)) ->
            Just color

        _ ->
            Color.fromString defaultColor


applyColor : String -> String -> StoryView Color view -> Payload -> Maybe view
applyColor name defaultColor storyView payload =
    Maybe.map2
        (|>)
        (getColor name defaultColor payload.state)
        (storyView payload)


getDate : String -> String -> State -> Maybe Date
getDate name defaultDate state =
    case Dict.get name state of
        Just (DateValue (Just date)) ->
            Just date

        _ ->
            Date.dateFromString defaultDate


applyDate : String -> String -> StoryView Date view -> Payload -> Maybe view
applyDate name defaultDate storyView payload =
    Maybe.map2
        (|>)
        (getDate name defaultDate payload.state)
        (storyView payload)


getTime : String -> String -> State -> Maybe Time
getTime name defaultTime state =
    case Dict.get name state of
        Just (TimeValue (Just time)) ->
            Just time

        _ ->
            Date.timeFromString defaultTime


applyTime : String -> String -> StoryView Time view -> Payload -> Maybe view
applyTime name defaultTime storyView payload =
    Maybe.map2
        (|>)
        (getTime name defaultTime payload.state)
        (storyView payload)


getFiles : String -> State -> List File
getFiles name state =
    case Dict.get name state of
        Just (FilesValue files) ->
            files

        _ ->
            []


applyFiles : String -> StoryView (List File) view -> Payload -> Maybe view
applyFiles name storyView payload =
    Maybe.map
        ((|>) (getFiles name payload.state))
        (storyView payload)


applyViewport : StoryView Viewport view -> Payload -> Maybe view
applyViewport storyView payload =
    Maybe.map
        ((|>) payload.viewport)
        (storyView payload)



-- S T A T E


type alias State =
    Dict String KnobValue


initial : State
initial =
    Dict.empty



-- U P D A T E


type Msg
    = UpdateBool String Bool
    | UpdateString String String
    | UpdateInt String (Limits Int) String
    | UpdateFloat String (Limits Float) String
    | UpdateColor String String
    | UpdateDate String String
    | UpdateTime String String
    | UpdateFiles String (List File)


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateBool name bool ->
            Dict.insert name (BoolValue bool) state

        UpdateString name string ->
            Dict.insert name (StringValue string) state

        UpdateInt name _ "" ->
            Dict.insert name (IntValue Nothing) state

        UpdateInt name limits str ->
            case Maybe.map (applyLimits limits) (String.toInt str) of
                Nothing ->
                    state

                Just int ->
                    Dict.insert name (IntValue (Just int)) state

        UpdateFloat name _ "" ->
            Dict.insert name (FloatValue Nothing) state

        UpdateFloat name limits str ->
            case Maybe.map (applyLimits limits) (String.toFloat str) of
                Nothing ->
                    state

                Just float ->
                    Dict.insert name (FloatValue (Just float)) state

        UpdateColor name "" ->
            Dict.insert name (ColorValue Nothing) state

        UpdateColor name str ->
            case Color.fromString str of
                Nothing ->
                    state

                Just color ->
                    Dict.insert name (ColorValue (Just color)) state

        UpdateDate name "" ->
            Dict.insert name (DateValue Nothing) state

        UpdateDate name str ->
            case Date.dateFromString str of
                Nothing ->
                    state

                Just date ->
                    Dict.insert name (DateValue (Just date)) state

        UpdateTime name "" ->
            Dict.insert name (TimeValue Nothing) state

        UpdateTime name str ->
            case Date.timeFromString str of
                Nothing ->
                    state

                Just time ->
                    Dict.insert name (TimeValue (Just time)) state

        UpdateFiles name files ->
            Dict.insert name (FilesValue files) state



-- V I E W


css : Style.Sheet
css =
    Style.sheet
        [ knob__root
        , knob__empty
        , knob__bool
        , knob__input
        , knob__select
        , Style.focusVisible knob__input
            [ Style.rule "box-shadow" ("0 0 0 2px " ++ Palette.gray05)
            ]
        , knob__radio_group
        , knob__radio_label
        , knob__radio_input
        , knob__row
        , knob__row_name
        , knob__row_knob
        ]


knob__root : Style.Selector
knob__root =
    Style.class "knob__root"
        [ Style.rule "width" "100%"
        , Style.rule "vertical-align" "middle"
        , Style.rule "border-collapse" "collapse"
        , Style.rule "color" Palette.dark
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font
        ]


knob__empty : Style.Selector
knob__empty =
    Style.class "knob__empty"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "flex"
        , Style.rule "justify-content" "center"
        , Style.rule "align-items" "center"
        , Style.rule "padding" "12px"
        , Style.rule "width" "100%"
        , Style.rule "color" Palette.dark
        , Style.rule "font-size" "18px"
        , Style.rule "font-family" Palette.font
        ]


knob__bool : Style.Selector
knob__bool =
    Style.class "knob__bool"
        [ Style.rule "margin" "0"
        ]


knob__select : Style.Selector
knob__select =
    Style.class "knob__select"
        [ Style.rule "-webkit-appearance" "menulist"
        ]


knob__input : Style.Selector
knob__input =
    Style.class "knob__input"
        [ Style.rule "-webkit-appearance" "none"
        , Style.rule "box-sizing" "border-box"
        , Style.rule "display" "block"
        , Style.rule "margin" "0"
        , Style.rule "padding" "4px 8px"
        , Style.rule "border" ("1px solid " ++ Palette.gray05)
        , Style.rule "border-radius" "4px"
        , Style.rule "width" "100% !important"
        , Style.rule "background" "transparent"
        , Style.rule "height" "28px"
        , Style.rule "min-height" "28px"
        , Style.rule "text-align" "left"
        , Style.rule "font-size" "inherit"
        , Style.rule "font-family" "inherit"
        , Style.rule "outline" "none"
        ]


knob__radio_group : Style.Selector
knob__radio_group =
    Style.class "knob__radio_group"
        [ Style.rule "display" "flex"
        , Style.rule "flex-direction" "column"
        , Style.rule "align-items" "flex-start"
        , Style.rule "margin-top" "-8px"
        ]


knob__radio_label : Style.Selector
knob__radio_label =
    Style.class "knob__radio_label"
        [ Style.rule "display" "flex"
        , Style.rule "align-items" "center"
        , Style.rule "margin-top" "8px"
        , Style.rule "cursor" "pointer"
        , Style.rule "word-break" "break-word"
        ]


knob__radio_input : Style.Selector
knob__radio_input =
    Style.class "knob__radio_text"
        [ Style.rule "margin" "0 8px 0 0"
        ]


knob__row : Style.Selector
knob__row =
    Style.class "knob__row"
        [ Style.rule "border-bottom" ("1px solid " ++ Palette.smoke)
        ]


knob__row_name : Style.Selector
knob__row_name =
    Style.class "knob__row_name"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "min-width" "100px"
        , Style.rule "height" "40px"
        , Style.rule "white-space" "nowrap"
        , Style.rule "padding" "8px 4px 8px 12px"
        , Style.rule "font-weight" "bold"
        ]


knob__row_knob : Style.Selector
knob__row_knob =
    Style.class "knob__row_knob"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "width" "100%"
        , Style.rule "height" "40px"
        , Style.rule "padding" "8px 4px 8px 12px"
        ]


viewKnobBool : String -> Bool -> Html Msg
viewKnobBool name checked =
    input
        [ Style.className knob__bool
        , Attributes.type_ "checkbox"
        , Attributes.name name
        , Attributes.checked checked
        , Events.onCheck (UpdateBool name)
        ]
        []


viewKnobString : String -> String -> Html Msg
viewKnobString name value =
    textarea
        [ Style.className knob__input
        , Attributes.name name
        , Attributes.value value
        , Attributes.tabindex 0
        , Events.onInput (UpdateString name)
        ]
        []


viewKnobNumber :
    (String -> msg)
    -> (number -> String)
    -> String
    -> Maybe number
    -> Limits number
    -> Html msg
viewKnobNumber msg numToString name number limits =
    input
        (Style.className knob__input
            :: Attributes.type_ "number"
            :: Attributes.name name
            :: Attributes.value (Maybe.withDefault "" (Maybe.map numToString number))
            :: Attributes.tabindex 0
            :: Events.onInput msg
            :: List.filterMap identity
                [ Maybe.map (Attributes.min << numToString) limits.min
                , Maybe.map (Attributes.max << numToString) limits.max
                , Maybe.map (Attributes.step << numToString) limits.step
                ]
        )
        []


viewKnobRadio : String -> Maybe String -> List String -> Html Msg
viewKnobRadio name selected options =
    Keyed.node "div"
        [ Style.className knob__radio_group
        ]
        (List.map
            (\value ->
                ( value
                , label
                    [ Style.className knob__radio_label
                    ]
                    [ input
                        [ Style.className knob__radio_input
                        , Attributes.type_ "radio"
                        , Attributes.name name
                        , Attributes.value value
                        , Attributes.tabindex 0
                        , Attributes.checked (Just value == selected)
                        , Events.onCheck (\_ -> UpdateString name value)
                        ]
                        []
                    , text value
                    ]
                )
            )
            options
        )


viewKnobSelect : String -> Maybe String -> List String -> Html Msg
viewKnobSelect name selected options =
    Keyed.node "select"
        [ Style.className knob__input
        , Style.className knob__select
        , Attributes.name name
        , Attributes.tabindex 0
        , Attributes.value (Maybe.withDefault "" selected)
        , Events.onInput (UpdateString name)
        ]
        (List.map
            (\value ->
                ( value
                , option
                    [ Attributes.value value
                    , Attributes.tabindex 0
                    ]
                    [ text value
                    ]
                )
            )
            options
        )


viewKnobColor : String -> Maybe Color -> Html Msg
viewKnobColor name color =
    input
        [ Style.className knob__input
        , Attributes.type_ "color"
        , Attributes.name name
        , Attributes.value (Maybe.withDefault "" (Maybe.map .hex color))
        , Attributes.tabindex 0
        , Events.onInput (UpdateColor name)
        ]
        []


viewKnobDate : String -> Maybe Date -> Html Msg
viewKnobDate name date =
    input
        [ Style.className knob__input
        , Attributes.type_ "date"
        , Attributes.name name
        , Attributes.value (Maybe.withDefault "" (Maybe.map Date.dateToString date))
        , Attributes.tabindex 0
        , Events.onInput (UpdateDate name)
        ]
        []


viewKnobTime : String -> Maybe Time -> Html Msg
viewKnobTime name time =
    input
        [ Style.className knob__input
        , Attributes.type_ "time"
        , Attributes.name name
        , Attributes.value (Maybe.withDefault "" (Maybe.map Date.timeToString time))
        , Attributes.tabindex 0
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
        , Attributes.tabindex 0
        , Events.on "change" (Decode.map (UpdateFiles name) filesDecoder)
        ]
        []


viewKnobStoryViewport : Viewport -> Html msg
viewKnobStoryViewport { width, height } =
    div []
        [ textCode (px width)
        , text " × "
        , textCode (px height)
        ]


viewKnobRow : Viewport -> String -> Knob -> Maybe KnobValue -> ( String, Html Msg )
viewKnobRow globalViewport name knob value =
    ( name
    , tr
        [ Style.className knob__row
        ]
        [ td [ Style.className knob__row_name ] [ text name ]
        , td [ Style.className knob__row_knob ] [ viewKnob globalViewport name knob value ]
        ]
    )


viewKnob : Viewport -> String -> Knob -> Maybe KnobValue -> Html Msg
viewKnob globalViewport name knob value =
    case ( knob, value ) of
        ( Bool _, Just (BoolValue bool) ) ->
            viewKnobBool name bool

        ( Bool defaultBool, _ ) ->
            viewKnobBool name defaultBool

        ( String _, Just (StringValue string) ) ->
            viewKnobString name string

        ( String defaultString, _ ) ->
            viewKnobString name defaultString

        ( Int False _ limits, Just (IntValue int) ) ->
            viewKnobNumber (UpdateInt name limits) String.fromInt name int limits

        ( Int False defaultInt limits, _ ) ->
            viewKnobNumber (UpdateInt name limits) String.fromInt name (Just defaultInt) limits

        ( Int True defaultInt limits, Just (IntValue int) ) ->
            range (UpdateInt name limits)
                name
                String.fromInt
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
                , value = Maybe.withDefault defaultInt int
                }

        ( Int True defaultInt limits, _ ) ->
            range (UpdateInt name limits)
                name
                String.fromInt
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
                , value = defaultInt
                }

        ( Float False _ limits, Just (FloatValue float) ) ->
            viewKnobNumber (UpdateFloat name limits) String.fromFloat name float limits

        ( Float False defaultFloat limits, _ ) ->
            viewKnobNumber (UpdateFloat name limits) String.fromFloat name (Just defaultFloat) limits

        ( Float True defaultFloat limits, Just (FloatValue float) ) ->
            range (UpdateFloat name limits)
                name
                String.fromFloat
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 1 limits.max
                , step = Maybe.withDefault 0.01 limits.step
                , value = Maybe.withDefault defaultFloat float
                }

        ( Float True defaultFloat limits, _ ) ->
            range (UpdateFloat name limits)
                name
                String.fromFloat
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 1 limits.max
                , step = Maybe.withDefault 0.01 limits.step
                , value = defaultFloat
                }

        ( Radio options, Just (StringValue selected) ) ->
            viewKnobRadio name (Just selected) options

        ( Radio options, _ ) ->
            viewKnobRadio name (List.head options) options

        ( Select options, Just (StringValue selected) ) ->
            viewKnobSelect name (Just selected) options

        ( Select options, _ ) ->
            viewKnobSelect name (List.head options) options

        ( Color _, Just (ColorValue (Just color)) ) ->
            viewKnobColor name (Just color)

        ( Color defaultColor, _ ) ->
            viewKnobColor name (Color.fromString defaultColor)

        ( Date _, Just (DateValue (Just date)) ) ->
            viewKnobDate name (Just date)

        ( Date defaultDate, _ ) ->
            viewKnobDate name (Date.dateFromString defaultDate)

        ( Time _, Just (TimeValue (Just time)) ) ->
            viewKnobTime name (Just time)

        ( Time defaultTime, _ ) ->
            viewKnobTime name (Date.timeFromString defaultTime)

        ( Files, _ ) ->
            viewKnobFile name

        ( StoryViewport, _ ) ->
            viewKnobStoryViewport globalViewport


viewEmpty : Html msg
viewEmpty =
    div
        [ Style.className knob__empty
        ]
        [ text "There are not declared Knobs to use."
        ]


viewRoot : Viewport -> List ( String, Knob ) -> State -> Html Msg
viewRoot globalViewport knobs state =
    knobs
        |> List.map
            (\( name, knob ) -> viewKnobRow globalViewport name knob (Dict.get name state))
        |> List.reverse
        |> Keyed.node "table" [ Style.className knob__root ]


view : Viewport -> State -> List ( String, Knob ) -> Html Msg
view globalViewport state knobs =
    if List.isEmpty knobs then
        viewEmpty

    else
        viewRoot globalViewport knobs state
