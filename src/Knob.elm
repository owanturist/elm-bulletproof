module Knob exposing
    ( Choice(..)
    , Knob(..)
    , KnobValue(..)
    , Limits
    , Msg
    , State
    , extract
    , initial
    , update
    , view
    )

import Color exposing (Color)
import Css
import Date exposing (Time)
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
import Time
import Utils exposing (Viewport, textCode)


type Knob
    = Bool Bool
    | String String
    | Int Bool Int (Limits Int)
    | Float Bool Float (Limits Float)
    | Choice Choice String (List String)
    | Color Color
    | Date Time.Posix
    | Time Time
    | Files
    | StoryViewport


type alias Limits x =
    { min : Maybe x
    , max : Maybe x
    , step : Maybe x
    }


type Choice
    = Radio
    | Select


type KnobValue
    = BoolValue Bool
    | StringValue String
    | IntValue (Maybe Int)
    | FloatValue (Maybe Float)
    | ColorValue (Maybe Color)
    | DateValue (Maybe Time.Posix)
    | TimeValue (Maybe Time)
    | FileValue (List File)


type alias State =
    Dict String KnobValue


initial : State
initial =
    Dict.empty


extract : String -> State -> Maybe KnobValue
extract =
    Dict.get



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
            case Date.posixFromString str of
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
            Dict.insert name (FileValue files) state



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


viewKnobRadio : String -> List String -> String -> Html Msg
viewKnobRadio name options current =
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
                        , Attributes.checked (value == current)
                        , Events.onCheck (\_ -> UpdateString name value)
                        ]
                    , styledRadioText value
                    ]
                )
            )
            options
        )


viewKnobSelect : String -> List String -> String -> Html Msg
viewKnobSelect name options current =
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
        [ Attributes.css cssInput
        , Attributes.type_ "color"
        , Attributes.name name
        , Attributes.value color
        , Attributes.tabindex 0
        , Events.onInput (UpdateColor name)
        ]
        []


viewKnobDate : String -> String -> Html Msg
viewKnobDate name value =
    input
        [ Attributes.css cssInput
        , Attributes.type_ "date"
        , Attributes.name name
        , Attributes.value value
        , Attributes.tabindex 0
        , Events.onInput (UpdateDate name)
        ]
        []


viewKnobTime : String -> String -> Html Msg
viewKnobTime name value =
    input
        [ Attributes.css cssInput
        , Attributes.type_ "time"
        , Attributes.name name
        , Attributes.value value
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


px : Int -> String
px n =
    String.fromInt n ++ "px"


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

        ( Choice Radio _ options, Just (StringValue string) ) ->
            viewKnobRadio name options string

        ( Choice Radio defaultString options, _ ) ->
            viewKnobRadio name options defaultString

        ( Choice Select _ options, Just (StringValue string) ) ->
            viewKnobSelect name options string

        ( Choice Select defaultValue options, _ ) ->
            viewKnobSelect name options defaultValue

        ( Color _, Just (ColorValue Nothing) ) ->
            viewKnobColor name ""

        ( Color _, Just (ColorValue (Just color)) ) ->
            viewKnobColor name color.hex

        ( Color defaultColor, _ ) ->
            viewKnobColor name defaultColor.hex

        ( Date _, Just (DateValue Nothing) ) ->
            viewKnobDate name ""

        ( Date _, Just (DateValue (Just date)) ) ->
            viewKnobDate name (Date.posixToString date)

        ( Date defaultPosix, _ ) ->
            viewKnobDate name (Date.posixToString defaultPosix)

        ( Time _, Just (TimeValue Nothing) ) ->
            viewKnobTime name ""

        ( Time _, Just (TimeValue (Just time)) ) ->
            viewKnobTime name (Date.timeToString time)

        ( Time defaultTime, _ ) ->
            viewKnobTime name (Date.timeToString defaultTime)

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
            (\( name, knob ) -> viewKnobRow globalViewport name knob (extract name state))
        |> List.reverse
        |> Keyed.node "table" [ Attributes.css cssRoot ]


view : Viewport -> List ( String, Knob ) -> State -> Html Msg
view globalViewport knobs state =
    if List.isEmpty knobs then
        viewEmpty

    else
        viewRoot globalViewport knobs state
