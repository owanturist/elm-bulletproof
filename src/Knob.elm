module Knob exposing
    ( Knob(..)
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
    , initial
    , update
    , view
    )

import Color exposing (Color)
import Css
import Date exposing (Date, Time)
import Dict exposing (Dict)
import File exposing (File)
import Html.Styled as Html exposing (Html, div, input, label, option, span, styled, td, text, textarea, tr)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import List
import Palette
import Range exposing (range)
import String
import Utils exposing (Viewport, px, textCode)


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
            Dict.insert name (BoolValue bool) state

        UpdateString name string ->
            Dict.insert name (StringValue string) state

        UpdateInt name "" ->
            Dict.insert name (IntValue Nothing) state

        UpdateInt name str ->
            case String.toInt str of
                Nothing ->
                    state

                Just int ->
                    Dict.insert name (IntValue (Just int)) state

        UpdateFloat name "" ->
            Dict.insert name (FloatValue Nothing) state

        UpdateFloat name str ->
            case String.toFloat str of
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


styledCheckbox : List (Html.Attribute msg) -> Html msg
styledCheckbox attributes =
    styled input
        [ Css.margin Css.zero
        ]
        (Attributes.type_ "checkbox" :: attributes)
        []


viewKnobBool : String -> Bool -> Html Msg
viewKnobBool name checked =
    styledCheckbox
        [ Attributes.name name
        , Attributes.checked checked
        , Events.onCheck (UpdateBool name)
        ]


cssInput : List Css.Style
cssInput =
    [ Css.property "-webkit-appearance" "none"
    , Css.boxSizing Css.borderBox
    , Css.display Css.block
    , Css.margin Css.zero
    , Css.padding2 (Css.px 4) (Css.px 8)
    , Css.border3 (Css.px 1) Css.solid Palette.gray50
    , Css.borderRadius (Css.px 4)
    , Css.important (Css.width (Css.pct 100))
    , Css.backgroundColor Css.transparent
    , Css.height (Css.px 28)
    , Css.minHeight (Css.px 28)
    , Css.textAlign Css.left
    , Css.fontFamily Css.inherit
    , Css.fontSize Css.inherit
    , Css.outline Css.none

    --
    , Css.focus
        [ Css.boxShadow5 Css.zero Css.zero Css.zero (Css.px 2) Palette.gray50
        ]

    --
    , Css.hover
        [ Css.boxShadow Css.none
        ]
    ]


viewKnobString : String -> String -> Html Msg
viewKnobString name value =
    textarea
        [ Attributes.css cssInput
        , Attributes.name name
        , Attributes.value value
        , Attributes.tabindex 0
        , Events.onInput (UpdateString name)
        ]
        []


viewKnobNumber :
    (String -> String -> msg)
    -> (number -> String)
    -> String
    -> Maybe number
    -> Limits number
    -> Html msg
viewKnobNumber msg numToString name number limits =
    input
        (Attributes.css cssInput
            :: Attributes.type_ "number"
            :: Attributes.name name
            :: Attributes.value (Maybe.withDefault "" (Maybe.map numToString number))
            :: Attributes.tabindex 0
            :: Events.onInput (msg name)
            :: List.filterMap identity
                [ Maybe.map (Attributes.min << numToString) limits.min
                , Maybe.map (Attributes.max << numToString) limits.max
                , Maybe.map (Attributes.step << numToString) limits.step
                ]
        )
        []


cssRadioGroup : List Css.Style
cssRadioGroup =
    [ Css.displayFlex
    , Css.flexDirection Css.column
    , Css.alignItems Css.flexStart
    , Css.marginTop (Css.px -8)
    ]


styledRadioLabel : List (Html msg) -> Html msg
styledRadioLabel =
    styled label
        [ Css.displayFlex
        , Css.alignItems Css.center
        , Css.marginTop (Css.px 8)
        , Css.cursor Css.pointer
        ]
        []


styledRadioText : String -> Html msg
styledRadioText =
    styled span
        [ Css.marginLeft (Css.px 8)
        ]
        []
        << List.singleton
        << text


styledRadio : List (Html.Attribute msg) -> Html msg
styledRadio attributes =
    styled input
        [ Css.margin Css.zero
        ]
        (Attributes.type_ "radio" :: attributes)
        []


viewKnobRadio : String -> Maybe String -> List String -> Html Msg
viewKnobRadio name selected options =
    Keyed.node "div"
        [ Attributes.css cssRadioGroup
        ]
        (List.map
            (\value ->
                ( value
                , styledRadioLabel
                    [ styledRadio
                        [ Attributes.name name
                        , Attributes.value value
                        , Attributes.tabindex 0
                        , Attributes.checked (Just value == selected)
                        , Events.onCheck (\_ -> UpdateString name value)
                        ]
                    , styledRadioText value
                    ]
                )
            )
            options
        )


viewKnobSelect : String -> Maybe String -> List String -> Html Msg
viewKnobSelect name selected options =
    Keyed.node "select"
        [ Attributes.css cssInput
        , Attributes.css [ Css.property "-webkit-appearance" "menulist" ]
        , Attributes.name name
        , Attributes.tabindex 0
        , Events.onInput (UpdateString name)
        ]
        (List.map
            (\value ->
                ( value
                , option
                    [ Attributes.value value
                    , Attributes.tabindex 0
                    , Attributes.selected (Just value == selected)
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
        [ Attributes.css cssInput
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
        [ Attributes.css cssInput
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
        [ Attributes.css cssInput
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


styledKnobRow : List (Html msg) -> Html msg
styledKnobRow =
    styled tr
        [ Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
        ]
        []


styledKnobName : List (Html msg) -> Html msg
styledKnobName =
    styled td
        [ Css.boxSizing Css.borderBox
        , Css.minWidth (Css.px 100)
        , Css.height (Css.px 40)
        , Css.whiteSpace Css.noWrap
        , Css.padding4 (Css.px 8) (Css.px 4) (Css.px 8) (Css.px 12)
        , Css.fontWeight Css.bold
        ]
        []


styledKnobCell : List (Html msg) -> Html msg
styledKnobCell =
    styled td
        [ Css.boxSizing Css.borderBox
        , Css.width (Css.pct 100)
        , Css.height (Css.px 40)
        , Css.padding4 (Css.px 8) (Css.px 12) (Css.px 8) (Css.px 4)
        ]
        []


viewKnobRow : Viewport -> String -> Knob -> Maybe KnobValue -> ( String, Html Msg )
viewKnobRow globalViewport name knob value =
    ( name
    , styledKnobRow
        [ styledKnobName [ text name ]
        , styledKnobCell [ viewKnob globalViewport name knob value ]
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
            viewKnobNumber UpdateInt String.fromInt name int limits

        ( Int False defaultInt limits, _ ) ->
            viewKnobNumber UpdateInt String.fromInt name (Just defaultInt) limits

        ( Int True defaultInt limits, Just (IntValue int) ) ->
            range (UpdateInt name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
                , value = Maybe.withDefault defaultInt int
                }

        ( Int True defaultInt limits, _ ) ->
            range (UpdateInt name)
                name
                String.fromInt
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 100 limits.max
                , step = Maybe.withDefault 1 limits.step
                , value = defaultInt
                }

        ( Float False _ limits, Just (FloatValue float) ) ->
            viewKnobNumber UpdateFloat String.fromFloat name float limits

        ( Float False defaultFloat limits, _ ) ->
            viewKnobNumber UpdateFloat String.fromFloat name (Just defaultFloat) limits

        ( Float True defaultFloat limits, Just (FloatValue float) ) ->
            range (UpdateFloat name)
                name
                String.fromFloat
                { min = Maybe.withDefault 0 limits.min
                , max = Maybe.withDefault 1 limits.max
                , step = Maybe.withDefault 0.01 limits.step
                , value = Maybe.withDefault defaultFloat float
                }

        ( Float True defaultFloat limits, _ ) ->
            range (UpdateFloat name)
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
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.padding (Css.px 12)
        , Css.width (Css.pct 100)
        , Css.color Palette.dark
        , Css.fontSize (Css.px 18)
        , Css.fontFamilies Palette.font
        ]
        []
        [ text "There are not declared Knobs to use."
        ]


cssRoot : List Css.Style
cssRoot =
    [ Css.width (Css.pct 100)
    , Css.verticalAlign Css.middle
    , Css.borderCollapse Css.collapse
    , Css.color Palette.dark
    , Css.fontSize (Css.px 13)
    , Css.fontFamilies Palette.font
    ]


viewRoot : Viewport -> List ( String, Knob ) -> State -> Html Msg
viewRoot globalViewport knobs state =
    knobs
        |> List.map
            (\( name, knob ) -> viewKnobRow globalViewport name knob (Dict.get name state))
        |> List.reverse
        |> Keyed.node "table" [ Attributes.css cssRoot ]


view : Viewport -> List ( String, Knob ) -> State -> Html Msg
view globalViewport knobs state =
    if List.isEmpty knobs then
        viewEmpty

    else
        viewRoot globalViewport knobs state
