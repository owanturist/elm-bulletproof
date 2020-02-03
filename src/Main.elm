module Main exposing (Program, run)

import AVL.Dict as Dict exposing (Dict)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Button exposing (button)
import Css
import Css.Global exposing (global)
import Css.Transitions exposing (transition)
import Error
import Html.Styled as Html exposing (Html, div, nav, styled, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Icon
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode exposing (encode)
import Knob
import Navigation
import Palette
import Renderer exposing (Renderer(..))
import Router
import Settings exposing (Orientation(..), Settings)
import Story exposing (Story(..))
import Task
import Url exposing (Url)
import Utils exposing (ifelse)



-- M O D E L


type Dragging
    = NoDragging
    | NavigationResizing Int Int
    | DockResizing Int Int


type alias Viewport =
    { width : Int
    , height : Int
    }


type alias State =
    { key : Browser.Navigation.Key
    , viewport : Viewport
    , store : Story.Store
    , current : Story.Path
    , dragging : Dragging
    , navigation : Navigation.Model
    }


type Model
    = Model Settings State (Dict Story.Path Knob.State)


init : List (Story Never Renderer) -> Maybe String -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories settingsJSON url key =
    let
        initialSettings =
            settingsJSON
                |> Maybe.andThen (Result.toMaybe << decodeString Settings.decoder)
                |> Maybe.withDefault Settings.default

        store =
            Story.makeStore stories

        ( initialStoryPath, initialCmd ) =
            case Router.parse url of
                Router.ToStory storyPath ->
                    ( storyPath, Cmd.none )

                Router.ToNotFound ->
                    ( []
                    , store.first
                        |> Maybe.withDefault []
                        |> Router.ToStory
                        |> Router.replace key
                    )

        initialFolderPath =
            List.take (List.length initialStoryPath - 1) initialStoryPath
    in
    ( Model
        initialSettings
        { key = key
        , viewport = Viewport 0 0
        , store = store
        , current = initialStoryPath
        , dragging = NoDragging
        , navigation = Navigation.open initialFolderPath Navigation.initial
        }
        Dict.empty
    , Cmd.batch
        [ initialCmd
        , Task.perform
            (\{ scene } -> ViewportChanged (round scene.width) (round scene.height))
            Browser.Dom.getViewport
        ]
    )



-- U P D A T E


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ViewportChanged Int Int
    | ToggleFullscreen
    | ToggleNavigationVisibility
    | ToggleDockVisibility
    | ToggleDockOrientation
    | ToggleGrid
    | TogglePaddings
    | ToggleBackground
    | StartNavigationResizing Int
    | StartDockResizing Int Int
    | Drag Int
    | DragEnd
    | GoToPrevStory
    | GoToNextStory
    | NavigationMsg Navigation.Msg
    | KnobMsg Story.Path Knob.Msg


saveSettings : (String -> Cmd msg) -> Settings -> Cmd Msg
saveSettings onSettingsChange settings =
    Settings.encoder settings
        |> encode 0
        |> onSettingsChange
        |> Cmd.map (always NoOp)


update : (String -> Cmd msg) -> Msg -> Model -> ( Model, Cmd Msg )
update onSettingsChange msg (Model settings state knobs) =
    case msg of
        NoOp ->
            ( Model settings state knobs, Cmd.none )

        UrlRequested (Browser.Internal url) ->
            ( Model settings state knobs
            , Browser.Navigation.pushUrl state.key (Url.toString url)
            )

        UrlRequested (Browser.External path) ->
            ( Model settings state knobs
            , Browser.Navigation.load path
            )

        UrlChanged url ->
            ( case Router.parse url of
                Router.ToStory storyPath ->
                    Model settings { state | current = storyPath } knobs

                Router.ToNotFound ->
                    Model settings { state | current = [] } knobs
            , Cmd.none
            )

        ViewportChanged width height ->
            ( Model settings { state | viewport = Viewport width height } knobs
            , Cmd.none
            )

        ToggleFullscreen ->
            let
                nextSettings =
                    if not settings.navigationVisible && not settings.dockVisible then
                        -- it seems like fullscreen
                        { settings | fullscreen = False, navigationVisible = True, dockVisible = True }

                    else
                        -- just toggle it
                        { settings | fullscreen = not settings.fullscreen }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        ToggleNavigationVisibility ->
            let
                nextSettings =
                    if settings.fullscreen then
                        -- if it's fullscreen user expects turn visibility on
                        { settings | fullscreen = False, navigationVisible = True, dockVisible = False }

                    else
                        -- just toggle it
                        { settings | navigationVisible = not settings.navigationVisible }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        ToggleDockVisibility ->
            let
                nextSettings =
                    if settings.fullscreen then
                        -- if it's fullscreen user expects turn visibility on
                        { settings | fullscreen = False, navigationVisible = False, dockVisible = True }

                    else
                        -- just toggle it
                        { settings | dockVisible = not settings.dockVisible }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        ToggleDockOrientation ->
            let
                nextSettings =
                    case settings.dockOrientation of
                        Horizontal ->
                            let
                                nextDockWidth =
                                    min
                                        settings.dockWidth
                                        (state.viewport.width - Settings.minStoryWidth - settings.navigationWidth)
                            in
                            { settings | dockOrientation = Vertical, dockWidth = nextDockWidth }

                        Vertical ->
                            { settings | dockOrientation = Horizontal }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        ToggleGrid ->
            let
                nextSettings =
                    { settings | showGrid = not settings.showGrid }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        TogglePaddings ->
            let
                nextSettings =
                    { settings | addPaddings = not settings.addPaddings }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        ToggleBackground ->
            let
                nextSettings =
                    { settings | darkBackground = not settings.darkBackground }
            in
            ( Model nextSettings state knobs
            , saveSettings onSettingsChange nextSettings
            )

        StartNavigationResizing start ->
            ( Model settings { state | dragging = NavigationResizing settings.navigationWidth start } knobs
            , Cmd.none
            )

        StartDockResizing x y ->
            ( case settings.dockOrientation of
                Horizontal ->
                    Model settings { state | dragging = DockResizing settings.dockHeight y } knobs

                Vertical ->
                    Model settings { state | dragging = DockResizing settings.dockWidth x } knobs
            , Cmd.none
            )

        Drag end ->
            ( case state.dragging of
                NoDragging ->
                    Model settings state knobs

                NavigationResizing initial start ->
                    let
                        nextNavigationWidth =
                            clamp
                                Settings.minNavigationWidth
                                (state.viewport.width - Settings.minStoryWidth - Settings.minDockWidth)
                                (initial + end - start)

                        nextDockWidth =
                            case settings.dockOrientation of
                                Horizontal ->
                                    settings.dockWidth

                                Vertical ->
                                    min
                                        settings.dockWidth
                                        (state.viewport.width - Settings.minStoryWidth - nextNavigationWidth)
                    in
                    Model { settings | navigationWidth = nextNavigationWidth, dockWidth = nextDockWidth } state knobs

                DockResizing initial start ->
                    case settings.dockOrientation of
                        Horizontal ->
                            let
                                nextDockHeight =
                                    clamp
                                        Settings.minDockHeight
                                        (state.viewport.height - Settings.minStoryHeight)
                                        (initial + start - end)
                            in
                            Model { settings | dockHeight = nextDockHeight } state knobs

                        Vertical ->
                            let
                                nextDockWidth =
                                    clamp
                                        Settings.minDockWidth
                                        (state.viewport.width - Settings.minStoryWidth - Settings.minNavigationWidth)
                                        (initial + start - end)

                                nextNavigationWidth =
                                    min
                                        settings.navigationWidth
                                        (state.viewport.width - Settings.minStoryWidth - nextDockWidth)
                            in
                            Model { settings | dockWidth = nextDockWidth, navigationWidth = nextNavigationWidth } state knobs
            , Cmd.none
            )

        DragEnd ->
            ( Model settings { state | dragging = NoDragging } knobs
            , saveSettings onSettingsChange settings
            )

        GoToPrevStory ->
            case Story.prev state.current state.store of
                Nothing ->
                    ( Model settings state knobs
                    , Cmd.none
                    )

                Just prevStoryPath ->
                    ( Model settings { state | navigation = Navigation.open prevStoryPath state.navigation } knobs
                    , Router.push state.key (Router.ToStory prevStoryPath)
                    )

        GoToNextStory ->
            case Story.next state.current state.store of
                Nothing ->
                    ( Model settings state knobs
                    , Cmd.none
                    )

                Just nextStoryPath ->
                    ( Model settings { state | navigation = Navigation.open nextStoryPath state.navigation } knobs
                    , Router.push state.key (Router.ToStory nextStoryPath)
                    )

        NavigationMsg msgOfNavigation ->
            let
                ( nextNavigation, cmdOfNavigation ) =
                    Navigation.update state.key msgOfNavigation state.navigation
            in
            ( Model settings { state | navigation = nextNavigation } knobs
            , Cmd.map NavigationMsg cmdOfNavigation
            )

        KnobMsg path msgOfKnob ->
            let
                storyKnobs =
                    Dict.get path knobs
                        |> Maybe.withDefault Knob.initial
                        |> Knob.update msgOfKnob
            in
            ( Model settings state (Dict.insert path storyKnobs knobs)
            , Cmd.none
            )



-- S U B S C R I P T I O N S


keyCodeToMsg : Int -> Maybe Msg
keyCodeToMsg keyCode =
    case Char.fromCode keyCode of
        'k' ->
            Just GoToPrevStory

        'j' ->
            Just GoToNextStory

        'o' ->
            Just ToggleDockOrientation

        'p' ->
            Just TogglePaddings

        'g' ->
            Just ToggleGrid

        'b' ->
            Just ToggleBackground

        'n' ->
            Just ToggleNavigationVisibility

        'd' ->
            Just ToggleDockVisibility

        'f' ->
            Just ToggleFullscreen

        _ ->
            Nothing


keyNavigationDecoder : Decoder Msg
keyNavigationDecoder =
    Decode.at [ "target", "tagName" ] Decode.string
        |> Decode.andThen
            (\tagName ->
                if List.member tagName [ "INPUT", "TEXTAREA" ] then
                    Decode.fail "Unhandled"

                else
                    Decode.field "keyCode" Decode.int
            )
        |> Decode.andThen
            (keyCodeToMsg
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "Unhandled")
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportChanged
        , Browser.Events.onKeyPress keyNavigationDecoder
        ]



-- V I E W


screenX : Decoder Int
screenX =
    Decode.field "screenX" Decode.int


screenY : Decoder Int
screenY =
    Decode.field "screenY" Decode.int


styledStoryScroller : List (Html msg) -> Html msg
styledStoryScroller =
    styled div
        [ Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.overflow Css.auto
        ]
        []


styledStoryContainer : Settings -> List (Html msg) -> Html msg
styledStoryContainer settings =
    styled div
        [ Css.all Css.initial
        , Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.justifyContent Css.flexStart
        , Css.position Css.relative
        , Css.padding (Css.px (ifelse settings.addPaddings 10 0))
        , Css.minWidth (Css.pct 100)
        , Css.minHeight (Css.pct 100)
        , Css.backgroundColor (ifelse settings.darkBackground Palette.dark Palette.white)
        , if settings.showGrid then
            Css.batch (cssGrid settings)

          else
            Css.batch []
        ]
        []


linearGradient : Float -> String -> String
linearGradient angle color =
    [ "linear-gradient("
    , String.join ","
        [ String.fromFloat angle ++ "deg"
        , color ++ " 0px"
        , color ++ " 1px"
        , "transparent 1px"
        , "transparent 100%"
        ]
    , ")"
    ]
        |> String.concat


cssGrid : Settings -> List Css.Style
cssGrid settings =
    let
        ( smallColor, bigColor ) =
            if settings.darkBackground then
                ( "#444", "#666" )

            else
                ( "#eee", "#ccc" )

        shift =
            ifelse settings.addPaddings 10 0
    in
    [ Css.backgroundPosition2 (Css.px (toFloat shift)) (Css.px (toFloat shift))

    --
    , [ "100px 100px"
      , "100px 100px"
      , "10px 10px"
      , "10px 10px"
      ]
        |> String.join ","
        |> Css.property "background-size"

    --
    , [ linearGradient 0 bigColor
      , linearGradient 270 bigColor
      , linearGradient 0 smallColor
      , linearGradient 270 smallColor
      ]
        |> String.join ","
        |> Css.property "background-image"
    ]


viewDragger : Orientation -> List (Html.Attribute msg) -> Html msg
viewDragger orientation attributes =
    styled div
        [ Css.position Css.absolute
        , case orientation of
            Horizontal ->
                Css.batch
                    [ Css.top (Css.px -3)
                    , Css.right Css.zero
                    , Css.left Css.zero
                    , Css.height (Css.px 4)
                    , Css.cursor Css.nsResize
                    ]

            Vertical ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.bottom Css.zero
                    , Css.left (Css.px -3)
                    , Css.width (Css.px 4)
                    , Css.cursor Css.ewResize
                    ]
        ]
        attributes
        []


styledDock : Settings -> Dragging -> List (Html msg) -> Html msg
styledDock settings dragging =
    let
        visible =
            not settings.fullscreen && settings.dockVisible

        ( ruleBorder, transitionRule, sizeStyle ) =
            case settings.dockOrientation of
                Horizontal ->
                    ( Css.borderTop3
                    , Css.Transitions.height 150
                    , Attributes.style "height" (String.fromInt (ifelse visible settings.dockHeight 0) ++ "px")
                    )

                Vertical ->
                    ( Css.borderLeft3
                    , Css.Transitions.width 150
                    , Attributes.style "width" (String.fromInt (ifelse visible settings.dockWidth 0) ++ "px")
                    )
    in
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.relative
        , ruleBorder (Css.px 2) Css.solid Palette.smoke
        , case dragging of
            DockResizing _ _ ->
                transition []

            _ ->
                transition [ transitionRule ]
        ]
        [ sizeStyle
        ]


styledDockHeader : List (Html msg) -> Html msg
styledDockHeader =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.padding2 (Css.px 8) (Css.px 12)
        , Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
        ]
        []


styledDockBody : List (Html msg) -> Html msg
styledDockBody =
    styled div
        [ Css.overflow Css.auto
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.padding2 Css.zero (Css.px 12)
        ]
        []


cssNextButton : List Css.Style
cssNextButton =
    [ Css.marginLeft (Css.px 4)
    ]


viewToggleGrid : Bool -> Html Msg
viewToggleGrid showGrid =
    button
        { onPress = ToggleGrid
        , dark = showGrid
        }
        [ Attributes.title (ifelse showGrid "(g) Hide background grid" "(g) Show background grid")
        ]
        [ Icon.grid
        ]


viewToggleBackground : Bool -> Html Msg
viewToggleBackground darkBackground =
    button
        { onPress = ToggleBackground
        , dark = darkBackground
        }
        [ Attributes.title (ifelse darkBackground "(b) Set light background" "(b) Set dark background")
        , Attributes.css cssNextButton
        ]
        [ Icon.fillDrop
        ]


viewTogglePaddings : Bool -> Html Msg
viewTogglePaddings addPaddings =
    button
        { onPress = TogglePaddings
        , dark = addPaddings
        }
        [ Attributes.title (ifelse addPaddings "(p) Remove paddings" "(p) Add paddings")
        , Attributes.css cssNextButton
        ]
        [ Icon.bordersBold
        ]


viewToggleDockOrientation : Orientation -> Html Msg
viewToggleDockOrientation dockOrientation =
    let
        ( title, icon ) =
            case dockOrientation of
                Horizontal ->
                    ( "(o) Dock to right", Icon.dockHorizontal )

                Vertical ->
                    ( "(o) Dock to bottom", Icon.dockVertical )
    in
    button
        { onPress = ToggleDockOrientation
        , dark = False
        }
        [ Attributes.title title
        , Attributes.css cssNextButton
        ]
        [ icon
        ]


viewDock : Settings -> Dragging -> Html Msg -> Html Msg
viewDock settings dragging knobs =
    styledDock
        settings
        dragging
        [ viewDragger settings.dockOrientation
            [ Events.on "mousedown" (Decode.map2 StartDockResizing screenX screenY)
            ]
        , styledDockHeader
            [ viewToggleGrid settings.showGrid
            , viewToggleBackground settings.darkBackground
            , viewTogglePaddings settings.addPaddings
            , viewToggleDockOrientation settings.dockOrientation
            ]
        , styledDockBody
            [ knobs
            ]
        ]


styledWorkspace : Orientation -> List (Html msg) -> Html msg
styledWorkspace dockOrientation =
    styled div
        [ Css.position Css.relative
        , Css.displayFlex
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.minWidth Css.zero
        , Css.backgroundColor Palette.white
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        , case dockOrientation of
            Horizontal ->
                Css.flexDirection Css.column

            Vertical ->
                Css.flexDirection Css.row
        ]
        []


viewWorkspace : Story.Payload Renderer -> Settings -> State -> Knob.State -> Html Msg
viewWorkspace payload settings state knobs =
    styledWorkspace settings.dockOrientation
        [ if settings.navigationVisible && not settings.fullscreen then
            viewDragger Vertical
                [ Events.on "mousedown" (Decode.map StartNavigationResizing screenX)
                ]

          else
            text ""

        --
        , styledStoryScroller
            [ styledStoryContainer settings
                [ Html.map (always NoOp) (Renderer.unwrap (payload.view knobs))
                ]
            ]

        --
        , Knob.view payload.knobs knobs
            |> Html.map (KnobMsg state.current)
            |> viewDock settings state.dragging
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


styledGlobal : Settings -> Dragging -> Html msg
styledGlobal settings dragging =
    global
        [ Css.Global.body
            [ Css.backgroundColor Palette.cloud
            ]
        , Css.Global.each
            [ Css.Global.html
            , Css.Global.body
            ]
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.minHeight (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        , case dragging of
            NoDragging ->
                Css.Global.everything []

            NavigationResizing _ _ ->
                Css.Global.everything
                    [ Css.property "user-select" "none !important"
                    , Css.cursor Css.ewResize |> Css.important
                    ]

            DockResizing _ _ ->
                Css.Global.everything
                    [ Css.property "user-select" "none !important"
                    , case settings.dockOrientation of
                        Horizontal ->
                            Css.cursor Css.nsResize |> Css.important

                        Vertical ->
                            Css.cursor Css.ewResize |> Css.important
                    ]
        ]


viewRoot : Settings -> Dragging -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewRoot settings dragging attributes children =
    styled div
        [ Css.position Css.relative
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        ]
        attributes
        (styledGlobal settings dragging :: children)


styledNavigation : Settings -> Dragging -> List (Html msg) -> Html msg
styledNavigation settings dragging =
    let
        visible =
            not settings.fullscreen && settings.navigationVisible
    in
    styled nav
        [ case dragging of
            NavigationResizing _ _ ->
                transition []

            _ ->
                transition
                    [ Css.Transitions.width 150
                    ]
        ]
        [ Attributes.style "width" (String.fromInt (ifelse visible settings.navigationWidth 0) ++ "px")
        ]


styledMenuTrigger : List (Html msg) -> Html msg
styledMenuTrigger =
    styled div
        [ Css.position Css.absolute
        , Css.top Css.zero
        , Css.left Css.zero
        , Css.zIndex (Css.int 2)
        , Css.margin (Css.px 12)
        ]
        []


viewMenuTrigger : Settings -> Html Msg
viewMenuTrigger settings =
    let
        vivid =
            not settings.fullscreen && settings.navigationVisible
    in
    styledMenuTrigger
        [ button
            { dark = False
            , onPress = ToggleFullscreen
            }
            [ Attributes.css
                [ Css.opacity (ifelse vivid (Css.num 1) (Css.num 0.2))

                --
                , transition
                    [ Css.Transitions.opacity (ifelse vivid 200 1000)
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
        ]


view : List (Story Never Renderer) -> Model -> Browser.Document Msg
view stories (Model settings state knobs) =
    let
        attrs =
            case state.dragging of
                NoDragging ->
                    []

                NavigationResizing _ _ ->
                    [ Events.on "mousemove" (Decode.map Drag screenX)
                    , Events.on "mouseup" (Decode.succeed DragEnd)
                    , Events.on "mouseleave" (Decode.succeed DragEnd)
                    ]

                DockResizing _ _ ->
                    [ case settings.dockOrientation of
                        Horizontal ->
                            Events.on "mousemove" (Decode.map Drag screenY)

                        Vertical ->
                            Events.on "mousemove" (Decode.map Drag screenX)
                    , Events.on "mouseup" (Decode.succeed DragEnd)
                    , Events.on "mouseleave" (Decode.succeed DragEnd)
                    ]
    in
    Browser.Document "Bulletproof"
        [ viewRoot
            settings
            state.dragging
            attrs
            [ viewMenuTrigger settings
            , styledNavigation
                settings
                state.dragging
                [ Html.map NavigationMsg (Navigation.view state.current stories state.navigation)
                ]
            , case Story.get state.current state.store of
                Nothing ->
                    viewEmpty

                Just payload ->
                    knobs
                        |> Dict.get state.current
                        |> Maybe.withDefault Knob.initial
                        |> viewWorkspace payload settings state
            ]
            |> Html.toUnstyled
        ]


type alias Program =
    Platform.Program (Maybe String) Model Msg


run : (String -> Cmd msg) -> List (Story Error.Reason Renderer) -> Program
run onSettingsChange dangerousStories =
    let
        ( initialisator, document ) =
            case Error.validateStories [] dangerousStories of
                Err errors ->
                    ( init []
                    , \(Model settings _ _) ->
                        Browser.Document "Error"
                            [ viewRoot settings
                                NoDragging
                                []
                                [ Error.view errors
                                ]
                                |> Html.toUnstyled
                            ]
                    )

                Ok stories ->
                    ( init stories
                    , view stories
                    )
    in
    Browser.application
        { init = initialisator
        , update = update onSettingsChange
        , view = document
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
