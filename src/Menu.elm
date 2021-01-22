module Menu exposing (Msg, Stage(..), subscriptions, update, view)

import Browser.Events
import Button exposing (button)
import Css
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (Html, div, span, styled, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Html.Styled.Lazy as Lazy
import Icon
import Json.Decode as Decode exposing (Decoder)
import Palette
import Settings exposing (Orientation(..), Settings)
import Utils exposing (ifelse, notClosest, onSpaceOrEnter)



-- U P D A T E


type Stage
    = Opened
    | Closed
    | PrevStory
    | NextStory
    | SettingsChanged Settings


type Msg
    = Open
    | Close
    | GoToPrevStory
    | GoToNextStory
    | ToggleFullscreen
    | ToggleNavigationVisibility
    | ToggleDockVisibility
    | ToggleDockOrientation
    | ToggleGrid
    | TogglePaddings
    | ToggleBackground


update : Msg -> Settings -> Stage
update msg settings =
    case msg of
        Open ->
            Opened

        Close ->
            Closed

        GoToPrevStory ->
            PrevStory

        GoToNextStory ->
            NextStory

        ToggleFullscreen ->
            if settings.navigationVisible || settings.dockVisible then
                SettingsChanged { settings | navigationVisible = False, dockVisible = False }

            else
                SettingsChanged { settings | navigationVisible = True, dockVisible = True }

        ToggleNavigationVisibility ->
            SettingsChanged { settings | navigationVisible = not settings.navigationVisible }

        ToggleDockVisibility ->
            SettingsChanged { settings | dockVisible = not settings.dockVisible }

        ToggleDockOrientation ->
            case settings.dockOrientation of
                Horizontal ->
                    SettingsChanged { settings | dockOrientation = Vertical }

                Vertical ->
                    SettingsChanged { settings | dockOrientation = Horizontal }

        ToggleGrid ->
            SettingsChanged { settings | showGrid = not settings.showGrid }

        TogglePaddings ->
            SettingsChanged { settings | addPaddings = not settings.addPaddings }

        ToggleBackground ->
            SettingsChanged { settings | darkBackground = not settings.darkBackground }



-- S U B S C R I P T I O N S


keyCodeDecoder : Int -> Decoder Msg
keyCodeDecoder keyCode =
    case Char.fromCode keyCode of
        'k' ->
            Decode.succeed GoToPrevStory

        'j' ->
            Decode.succeed GoToNextStory

        'o' ->
            Decode.succeed ToggleDockOrientation

        'p' ->
            Decode.succeed TogglePaddings

        'g' ->
            Decode.succeed ToggleGrid

        'b' ->
            Decode.succeed ToggleBackground

        's' ->
            Decode.succeed ToggleNavigationVisibility

        'd' ->
            Decode.succeed ToggleDockVisibility

        'f' ->
            Decode.succeed ToggleFullscreen

        _ ->
            Decode.fail "Unhandled"


keyNavigationDecoder : Decoder Msg
keyNavigationDecoder =
    Decode.andThen
        (\tagName ->
            if List.member tagName [ "INPUT", "TEXTAREA" ] then
                Decode.fail "Unhandled"

            else
                Decode.andThen keyCodeDecoder (Decode.field "keyCode" Decode.int)
        )
        (Decode.at [ "target", "tagName" ] Decode.string)


subscriptions : Bool -> Sub Msg
subscriptions opened =
    Sub.batch
        [ Browser.Events.onKeyPress keyNavigationDecoder
        , if opened then
            Browser.Events.onMouseUp (notClosest className Close)

          else
            Sub.none
        ]



-- V I E W


className : String
className =
    "__bp-menu-dropdown__"


viewTrigger : Bool -> Msg -> Html Msg
viewTrigger solid onClick =
    button onClick
        [ Attributes.css
            [ Css.opacity (ifelse solid (Css.num 1) (Css.num 0.2))

            --
            , transition
                [ Css.Transitions.opacity (ifelse solid 200 1000)
                ]

            --
            , Css.hover
                [ Css.opacity (Css.num 1)
                , transition
                    [ Css.Transitions.opacity 200
                    ]
                ]

            --
            , Css.focus
                [ Css.opacity (Css.num 1)
                , transition
                    [ Css.Transitions.opacity 200
                    ]
                ]
            ]
        ]
        [ Icon.bars
        ]


styledDropdown : List (Html msg) -> Html msg
styledDropdown =
    styled div
        [ Css.position Css.absolute
        , Css.top (Css.pct 100)
        , Css.left Css.zero
        , Css.marginTop (Css.px 8)
        , Css.padding2 (Css.px 4) Css.zero
        , Css.border3 (Css.px 1) Css.solid Palette.gray
        , Css.borderRadius (Css.px 3)
        , Css.minWidth (Css.px 200)
        , Css.backgroundColor Palette.white
        , Css.boxShadow4 Css.zero (Css.px 3) (Css.px 4) Palette.gray50
        ]
        [ Attributes.class className
        ]


styledItem : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledItem attributes =
    styled div
        [ Css.displayFlex
        , Css.justifyContent Css.spaceBetween
        , Css.padding2 (Css.px 4) (Css.px 12)
        , Css.cursor Css.pointer
        , Css.outline Css.none

        --
        , Css.hover
            [ Css.backgroundColor Palette.smoke
            ]

        --
        , Css.focus
            [ Css.backgroundColor Palette.smoke
            ]
        ]
        (Attributes.attribute "role" "button"
            :: Attributes.tabindex 0
            :: attributes
        )


styledHotKey : List (Html msg) -> Html msg
styledHotKey =
    styled span
        [ Css.display Css.inlineBlock
        , Css.marginLeft (Css.px 4)
        , Css.padding2 (Css.px 2) (Css.px 4)
        , Css.borderRadius (Css.px 3)
        , Css.color Palette.gray
        , Css.backgroundColor Palette.smoke
        , Css.fontFamily Css.monospace
        , Css.fontSize (Css.px 14)
        , Css.lineHeight (Css.int 1)
        ]
        []


viewItem : msg -> Char -> String -> Html msg
viewItem msg key title =
    styledItem
        [ Events.onClick msg
        , onSpaceOrEnter msg
        ]
        [ text title
        , styledHotKey
            [ text (String.fromChar (Char.toUpper key))
            ]
        ]


viewDropdown : Settings -> Html Msg
viewDropdown settings =
    styledDropdown
        [ ifelse settings.navigationVisible "Hide sidebar" "Show sidebar"
            |> viewItem ToggleNavigationVisibility 's'

        --
        , ifelse settings.dockVisible "Hide dock" "Show dock"
            |> viewItem ToggleDockVisibility 'd'

        --
        , case settings.dockOrientation of
            Horizontal ->
                viewItem ToggleDockOrientation 'o' "Move dock to right"

            Vertical ->
                viewItem ToggleDockOrientation 'o' "Move dock to bottom"

        --
        , ifelse
            (settings.navigationVisible || settings.dockVisible)
            "Go to full screen"
            "Exit full screen"
            |> viewItem ToggleFullscreen 'f'

        --
        , ifelse settings.showGrid "Hide grid" "Show grid"
            |> viewItem ToggleGrid 'g'

        --
        , ifelse settings.addPaddings "Remove paddings" "Add paddings"
            |> viewItem TogglePaddings 'p'

        --
        , ifelse settings.darkBackground "Use white background" "Use dark background"
            |> viewItem ToggleBackground 'b'

        --
        , viewItem GoToPrevStory 'k' "Previous story"

        --
        , viewItem GoToNextStory 'j' "Next story"
        ]


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.position Css.relative
        , Css.property "user-select" "none"
        , Css.fontFamilies Palette.font
        , Css.fontSize (Css.px 13)
        ]
        []


view : Bool -> Settings -> Html Msg
view opened settings =
    if opened then
        styledRoot
            [ Lazy.lazy2 viewTrigger True Close
            , Lazy.lazy viewDropdown settings
            ]

    else
        styledRoot
            [ Lazy.lazy2 viewTrigger settings.navigationVisible Open
            ]
