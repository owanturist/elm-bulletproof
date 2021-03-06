module Menu exposing (Msg, Stage(..), css, subscriptions, update, view)

import Browser.Events
import Button exposing (button)
import Html exposing (Html, div, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy
import Icon
import Json.Decode as Decode exposing (Decoder)
import Palette
import Settings exposing (Orientation(..), Settings)
import Style
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
            Browser.Events.onMouseUp (notClosest (Style.className menu__dropdown) Close)

          else
            Sub.none
        ]



-- V I E W


css : Style.Sheet
css =
    Style.elements
        [ menu__root
        , menu__trigger
        , menu__trigger__vivid
        , menu__dropdown
        , menu__item
        , menu__hotkey
        ]


menu__root : Style.Element
menu__root =
    Style.el "menu__root"
        [ Style.rule "position" "relative"
        , Style.rule "user-select" "none"
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font
        ]


menu__trigger : Style.Element
menu__trigger =
    Style.el "menu__trigger"
        [ Style.rule "opacity" "0.2"
        , Style.rule "transition" "opacity 1s"
        ]
        |> Style.hover
            [ Style.rule "opacity" "1"
            , Style.rule "transition" "opacity 0.2s"
            ]
        |> Style.focusVisible
            [ Style.rule "opacity" "1"
            , Style.rule "transition" "opacity 0.2s"
            ]


menu__trigger__vivid : Style.Element
menu__trigger__vivid =
    [ Style.rule "opacity" "1"
    , Style.rule "transition" "opacity 0.2s"
    ]
        |> Style.mod menu__trigger "vivid"


menu__dropdown : Style.Element
menu__dropdown =
    Style.el "menu__dropdown"
        [ Style.rule "position" "absolute"
        , Style.rule "top" "100%"
        , Style.rule "left" "0"
        , Style.rule "margin-top" "8px"
        , Style.rule "padding" "4px 0"
        , Style.rule "border" ("1px solid " ++ Palette.gray)
        , Style.rule "border-radius" "3px"
        , Style.rule "min-width" "200px"
        , Style.rule "background" Palette.white
        , Style.rule "box-shadow" ("0 3px 4px " ++ Palette.gray05)
        ]


menu__item : Style.Element
menu__item =
    Style.el "menu__item"
        [ Style.rule "display" "flex"
        , Style.rule "justify-content" "space-between"
        , Style.rule "padding" "4px 12px"
        , Style.rule "cursor" "pointer"
        , Style.rule "outline" "none"
        ]
        |> Style.hover
            [ Style.rule "background" Palette.smoke
            ]
        |> Style.focusVisible
            [ Style.rule "background" Palette.smoke
            ]


menu__hotkey : Style.Element
menu__hotkey =
    Style.el "menu__hotkey"
        [ Style.rule "display" "inline-block"
        , Style.rule "margin-left" "4px"
        , Style.rule "padding" "2px 4px"
        , Style.rule "border-radius" "3px"
        , Style.rule "color" Palette.gray
        , Style.rule "background" Palette.smoke
        , Style.rule "line-height" "1"
        , Style.rule "font-size" "14px"
        , Style.rule "font-family" "monospace"
        ]


viewTrigger : Bool -> Msg -> Html Msg
viewTrigger vivid onClick =
    button onClick
        [ Style.classList
            [ ( menu__trigger, True )
            , ( menu__trigger__vivid, vivid )
            ]
        ]
        [ Icon.bars
        ]


viewItem : msg -> Char -> String -> Html msg
viewItem msg key title =
    div
        [ Style.class menu__item
        , Attributes.attribute "role" "button"
        , Attributes.tabindex 0
        , Events.onClick msg
        , onSpaceOrEnter msg
        ]
        [ text title
        , span
            [ Style.class menu__hotkey
            ]
            [ text (String.fromChar (Char.toUpper key))
            ]
        ]


viewDropdown : Settings -> Html Msg
viewDropdown settings =
    div
        [ Style.class menu__dropdown
        ]
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


view : Bool -> Settings -> Html Msg
view opened settings =
    let
        children =
            if opened then
                [ Lazy.lazy2 viewTrigger True Close
                , Lazy.lazy viewDropdown settings
                ]

            else
                [ Lazy.lazy2 viewTrigger settings.navigationVisible Open
                ]
    in
    div [ Style.class menu__root ] children
