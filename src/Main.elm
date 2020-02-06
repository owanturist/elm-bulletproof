module Main exposing (Program, run)

import AVL.Dict as Dict exposing (Dict)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Css
import Css.Global exposing (global)
import Css.Transitions exposing (transition)
import Empty
import Error exposing (Error)
import Html.Styled as Html exposing (Html, div, nav, styled)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode exposing (encode)
import Knob
import Menu
import Navigation
import NotFound
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
    , menuOpened : Bool
    , store : Story.Store
    , current : Story.Path
    , dragging : Dragging
    , navigation : Navigation.Model
    }


type alias Model =
    { settings : Settings
    , state : State
    , knobs : Dict Story.Path Knob.State
    }


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
    in
    ( Model
        initialSettings
        { key = key
        , viewport = Viewport 0 0
        , menuOpened = False
        , store = store
        , current = initialStoryPath
        , dragging = NoDragging
        , navigation = Navigation.open initialStoryPath Navigation.initial
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
    | StartNavigationResizing Int
    | StartDockResizing Int Int
    | Drag Int
    | DragEnd
    | MenuMsg Menu.Msg
    | NavigationMsg Navigation.Msg
    | KnobMsg Story.Path Knob.Msg


saveSettings : (String -> Cmd msg) -> Settings -> Cmd Msg
saveSettings onSettingsChange settings =
    Settings.encoder settings
        |> encode 0
        |> onSettingsChange
        |> Cmd.map (always NoOp)


update : (String -> Cmd msg) -> Msg -> Model -> ( Model, Cmd Msg )
update onSettingsChange msg { settings, state, knobs } =
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

        MenuMsg msgOfMenu ->
            case Menu.update msgOfMenu settings of
                Menu.Opened ->
                    ( Model settings { state | menuOpened = True } knobs
                    , Cmd.none
                    )

                Menu.Closed ->
                    ( Model settings { state | menuOpened = False } knobs
                    , Cmd.none
                    )

                Menu.PrevStory ->
                    case Story.prev state.current state.store of
                        Nothing ->
                            ( Model settings state knobs
                            , Cmd.none
                            )

                        Just prevStoryPath ->
                            ( Model settings { state | navigation = Navigation.open prevStoryPath state.navigation } knobs
                            , Router.push state.key (Router.ToStory prevStoryPath)
                            )

                Menu.NextStory ->
                    case Story.next state.current state.store of
                        Nothing ->
                            ( Model settings state knobs
                            , Cmd.none
                            )

                        Just nextStoryPath ->
                            ( Model settings { state | navigation = Navigation.open nextStoryPath state.navigation } knobs
                            , Router.push state.key (Router.ToStory nextStoryPath)
                            )

                Menu.SettingsChanged nextSettings ->
                    ( Model nextSettings state knobs
                    , saveSettings onSettingsChange nextSettings
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


subscriptions : Model -> Sub Msg
subscriptions { state } =
    Sub.batch
        [ Browser.Events.onResize ViewportChanged
        , Sub.map MenuMsg (Menu.subscriptions state.menuOpened)
        ]



-- V I E W


screenX : Decoder Int
screenX =
    Decode.field "screenX" Decode.int


screenY : Decoder Int
screenY =
    Decode.field "screenY" Decode.int


styledStoryScroller : Settings -> State -> List (Html msg) -> Html msg
styledStoryScroller settings { viewport, dragging } =
    let
        ( width, height ) =
            case settings.dockOrientation of
                Horizontal ->
                    ( viewport.width - ifelse settings.navigationVisible settings.navigationWidth 0
                    , viewport.height - ifelse settings.dockVisible settings.dockHeight 0
                    )

                Vertical ->
                    ( viewport.width
                        - ifelse settings.navigationVisible settings.navigationWidth 0
                        - ifelse settings.dockVisible settings.dockWidth 0
                    , viewport.height
                    )
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.overflow Css.auto

        --
        , if dragging == NoDragging then
            transition
                [ Css.Transitions.width 150
                , Css.Transitions.height 150
                ]

          else
            Css.batch []
        ]
        [ Attributes.style "width" (String.fromInt width ++ "px")
        , Attributes.style "height" (String.fromInt height ++ "px")
        ]


styledStoryContainer : Settings -> List (Html msg) -> Html msg
styledStoryContainer settings =
    styled div
        [ Css.all Css.initial
        , Css.flex2 (Css.int 1) Css.zero
        , Css.flexBasis Css.auto
        , Css.display Css.block
        , Css.position Css.relative
        , Css.padding (Css.px (ifelse settings.addPaddings 10 0))
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


viewDragger : Orientation -> List Css.Style -> List (Html.Attribute msg) -> Html msg
viewDragger orientation styles attributes =
    styled div
        [ Css.position Css.absolute
        , case orientation of
            Horizontal ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.right Css.zero
                    , Css.left Css.zero
                    , Css.height (Css.px 4)
                    , Css.cursor Css.nsResize
                    ]

            Vertical ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.bottom Css.zero
                    , Css.left Css.zero
                    , Css.width (Css.px 4)
                    , Css.cursor Css.ewResize
                    ]
        , Css.batch styles
        ]
        attributes
        []


styledDockScroller : List (Html msg) -> Html msg
styledDockScroller =
    styled div
        [ Css.displayFlex
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.overflow Css.auto
        ]
        []


styledDock : Settings -> List (Html msg) -> Html msg
styledDock settings =
    styled div
        [ Css.position Css.relative
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.backgroundColor Palette.white
        , Css.overflow Css.hidden
        , case settings.dockOrientation of
            Horizontal ->
                Css.borderTop3 (Css.px 2) Css.solid Palette.smoke

            Vertical ->
                Css.borderLeft3 (Css.px 2) Css.solid Palette.smoke
        ]
        []


viewDock : Settings -> Html Msg -> Html Msg
viewDock settings knobs =
    styledDock settings
        [ viewDragger settings.dockOrientation
            []
            [ Events.on "mousedown" (Decode.map2 StartDockResizing screenX screenY)
            ]
        , styledDockScroller [ knobs ]
        ]


styledWorkspace : Settings -> State -> List (Html msg) -> Html msg
styledWorkspace settings { viewport, dragging } =
    let
        width =
            viewport.width - ifelse settings.navigationVisible settings.navigationWidth 0
    in
    styled div
        [ Css.position Css.relative
        , Css.displayFlex
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        , Css.flexDirection (ifelse (Settings.isHorizontal settings.dockOrientation) Css.column Css.row)

        --
        , if dragging == NoDragging then
            transition
                [ Css.Transitions.width 150
                ]

          else
            Css.batch []
        ]
        [ Attributes.style "width" (String.fromInt width ++ "px")
        ]


viewWorkspace : Story.Payload Renderer -> Settings -> State -> Knob.State -> Html Msg
viewWorkspace payload settings state knobs =
    styledWorkspace
        settings
        state
        [ styledStoryScroller
            settings
            state
            [ styledStoryContainer settings
                [ Html.map (always NoOp) (Renderer.unwrap (payload.view knobs))
                ]
            ]

        --
        , Knob.view payload.knobs knobs
            |> Html.map (KnobMsg state.current)
            |> viewDock settings
        ]


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
                    , ifelse (Settings.isHorizontal settings.dockOrientation) Css.nsResize Css.ewResize
                        |> Css.cursor
                        |> Css.important
                    ]
        ]


styledMenu : List (Html msg) -> Html msg
styledMenu =
    styled div
        [ Css.position Css.absolute
        , Css.top Css.zero
        , Css.left Css.zero
        , Css.zIndex (Css.int 2)
        , Css.margin (Css.px 12)
        ]
        []


styledNavigation : List (Html msg) -> Html msg
styledNavigation =
    styled nav
        [ Css.position Css.relative
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.overflow Css.hidden
        ]
        []


styledRoot : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.position Css.relative
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.fontFamilies Palette.font
        , Css.fontSize (Css.px 13)
        ]


viewRoot : Settings -> Dragging -> List (Html Msg) -> Html Msg
viewRoot settings dragging children =
    styledRoot
        (case dragging of
            NoDragging ->
                []

            NavigationResizing _ _ ->
                [ Events.on "mousemove" (Decode.map Drag screenX)
                , Events.on "mouseup" (Decode.succeed DragEnd)
                , Events.on "mouseleave" (Decode.succeed DragEnd)
                ]

            DockResizing _ _ ->
                [ ifelse (Settings.isHorizontal settings.dockOrientation) screenY screenX
                    |> Decode.map Drag
                    |> Events.on "mousemove"
                , Events.on "mouseup" (Decode.succeed DragEnd)
                , Events.on "mouseleave" (Decode.succeed DragEnd)
                ]
        )
        (styledGlobal settings dragging :: children)


viewBulletproof : List (Story Never Renderer) -> Model -> Browser.Document Msg
viewBulletproof stories { settings, state, knobs } =
    let
        ( actualSettings, workspaceView ) =
            case Story.get state.current state.store of
                Nothing ->
                    ( { settings | navigationVisible = True }
                    , styledWorkspace
                        { settings | navigationVisible = True }
                        state
                        [ NotFound.view state.current
                        ]
                    )

                Just payload ->
                    ( settings
                    , knobs
                        |> Dict.get state.current
                        |> Maybe.withDefault Knob.initial
                        |> viewWorkspace payload settings state
                    )
    in
    Browser.Document ("Bulletproof | " ++ String.join " / " state.current)
        [ viewRoot
            actualSettings
            state.dragging
            [ styledMenu
                [ Html.map MenuMsg (Menu.view state.menuOpened actualSettings)
                ]

            --
            , styledNavigation
                [ viewDragger Vertical
                    [ Css.zIndex (Css.int 2)
                    , Css.left Css.auto
                    , Css.right Css.zero
                    ]
                    [ Events.on "mousedown" (Decode.map StartNavigationResizing screenX)
                    ]
                , Html.map NavigationMsg (Navigation.view state.current stories state.navigation)
                ]

            --
            , workspaceView
            ]
            |> Html.toUnstyled
        ]


viewError : List Error -> Browser.Document msg
viewError errors =
    Browser.Document "Bulletproof | Warning"
        [ styledRoot
            []
            [ styledGlobal Settings.default NoDragging
            , Error.view errors
            ]
            |> Html.toUnstyled
        ]


viewEmpty : Browser.Document msg
viewEmpty =
    Browser.Document "Bulletproof"
        [ styledRoot
            []
            [ styledGlobal Settings.default NoDragging
            , Empty.view
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
                    , always (viewError errors)
                    )

                Ok [] ->
                    ( init []
                    , always viewEmpty
                    )

                Ok stories ->
                    ( init stories
                    , viewBulletproof stories
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
