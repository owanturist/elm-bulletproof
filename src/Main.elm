module Main exposing (Program, run)

import AVL.Dict as Dict exposing (Dict)
import Addons exposing (Addons)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Button exposing (button)
import Css
import Css.Global exposing (global)
import Html.Styled as Html exposing (Html, div, nav, styled, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Icon
import Json.Decode as Decode exposing (Decoder)
import Knob
import Navigation
import Palette
import Renderer exposing (Renderer(..))
import Router
import Story exposing (Story(..))
import Task
import Url exposing (Url)
import Utils exposing (ifelse)



-- S E T T I N G S


type alias Settings =
    { navigationWidth : Int
    , dockWidth : Int
    , dockHeight : Int
    , dockOrientation : Orientation
    , addPaddings : Bool
    , darkBackground : Bool
    , showGrid : Bool
    }


defaultSettings : Settings
defaultSettings =
    { navigationWidth = 200
    , dockWidth = 400
    , dockHeight = 300
    , dockOrientation = Horizontal
    , addPaddings = False
    , darkBackground = False
    , showGrid = False
    }


minNavigationWidth : Int
minNavigationWidth =
    100


minDockWidth : Int
minDockWidth =
    300


minDockHeight : Int
minDockHeight =
    200


minStoryWidth : Int
minStoryWidth =
    300


minStoryHeight : Int
minStoryHeight =
    200



-- M O D E L


type Orientation
    = Horizontal
    | Vertical


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
    = Model Settings State (Dict Story.Path Addons)


init : List (Story Renderer) -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories () url key =
    let
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
        defaultSettings
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
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ViewportChanged Int Int
    | ToggleDockOrientation
    | ToggleGrid
    | TogglePaddings
    | ToggleBackground
    | StartNavigationResizing Int
    | StartDockResizing Int Int
    | Drag Int
    | DragEnd
    | NavigationMsg Navigation.Msg
    | KnobMsg Story.Path Knob.Msg
    | StoryMsg ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model settings state addons) =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( Model settings state addons
            , Browser.Navigation.pushUrl state.key (Url.toString url)
            )

        UrlRequested (Browser.External path) ->
            ( Model settings state addons
            , Browser.Navigation.load path
            )

        ViewportChanged width height ->
            ( Model settings { state | viewport = Viewport width height } addons
            , Cmd.none
            )

        UrlChanged url ->
            ( case Router.parse url of
                Router.ToStory storyPath ->
                    Model settings { state | current = storyPath } addons

                Router.ToNotFound ->
                    Model settings { state | current = [] } addons
            , Cmd.none
            )

        ToggleDockOrientation ->
            ( case settings.dockOrientation of
                Horizontal ->
                    let
                        nextDockWidth =
                            min
                                settings.dockWidth
                                (state.viewport.width - minStoryWidth - settings.navigationWidth)
                    in
                    Model { settings | dockOrientation = Vertical, dockWidth = nextDockWidth } state addons

                Vertical ->
                    Model { settings | dockOrientation = Horizontal } state addons
            , Cmd.none
            )

        ToggleGrid ->
            ( Model { settings | showGrid = not settings.showGrid } state addons
            , Cmd.none
            )

        TogglePaddings ->
            ( Model { settings | addPaddings = not settings.addPaddings } state addons
            , Cmd.none
            )

        ToggleBackground ->
            ( Model { settings | darkBackground = not settings.darkBackground } state addons
            , Cmd.none
            )

        StartNavigationResizing start ->
            ( Model settings { state | dragging = NavigationResizing settings.navigationWidth start } addons
            , Cmd.none
            )

        StartDockResizing x y ->
            ( case settings.dockOrientation of
                Horizontal ->
                    Model settings { state | dragging = DockResizing settings.dockHeight y } addons

                Vertical ->
                    Model settings { state | dragging = DockResizing settings.dockWidth x } addons
            , Cmd.none
            )

        Drag end ->
            ( case state.dragging of
                NoDragging ->
                    Model settings state addons

                NavigationResizing initial start ->
                    let
                        nextNavigationWidth =
                            clamp
                                minNavigationWidth
                                (state.viewport.width - minStoryWidth - minDockWidth)
                                (initial + end - start)

                        nextDockWidth =
                            case settings.dockOrientation of
                                Horizontal ->
                                    settings.dockWidth

                                Vertical ->
                                    min
                                        settings.dockWidth
                                        (state.viewport.width - minStoryWidth - nextNavigationWidth)
                    in
                    Model { settings | navigationWidth = nextNavigationWidth, dockWidth = nextDockWidth } state addons

                DockResizing initial start ->
                    case settings.dockOrientation of
                        Horizontal ->
                            let
                                nextDockHeight =
                                    clamp
                                        minDockHeight
                                        (state.viewport.height - minStoryHeight)
                                        (initial + start - end)
                            in
                            Model { settings | dockHeight = nextDockHeight } state addons

                        Vertical ->
                            let
                                nextDockWidth =
                                    clamp
                                        minDockWidth
                                        (state.viewport.width - minStoryWidth - minNavigationWidth)
                                        (initial + start - end)

                                nextNavigationWidth =
                                    min
                                        settings.navigationWidth
                                        (state.viewport.width - minStoryWidth - nextDockWidth)
                            in
                            Model { settings | dockWidth = nextDockWidth, navigationWidth = nextNavigationWidth } state addons
            , Cmd.none
            )

        DragEnd ->
            ( Model settings { state | dragging = NoDragging } addons
            , Cmd.none
            )

        NavigationMsg msgOfNavigation ->
            let
                ( nextNavigation, cmdOfNavigation ) =
                    Navigation.update state.key msgOfNavigation state.navigation
            in
            ( Model settings { state | navigation = nextNavigation } addons
            , Cmd.map NavigationMsg cmdOfNavigation
            )

        KnobMsg path msgOfKnob ->
            let
                storyAddons =
                    Maybe.withDefault Addons.initial (Dict.get path addons)

                nextStoryAddons =
                    { storyAddons | knobs = Knob.update msgOfKnob storyAddons.knobs }
            in
            ( Model settings state (Dict.insert path nextStoryAddons addons)
            , Cmd.none
            )

        StoryMsg () ->
            ( Model settings state addons, Cmd.none )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ViewportChanged
        , Decode.field "keyCode" Decode.int
            |> Decode.andThen
                (\keyCode ->
                    let
                        _ =
                            Debug.log "" keyCode
                    in
                    Decode.fail "Unhandled"
                )
            |> Browser.Events.onKeyPress
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
        , Css.cursor Css.inherit
        ]
        []


styledStoryContainer : Settings -> List (Html msg) -> Html msg
styledStoryContainer settings =
    styled div
        [ Css.all Css.initial
        , Css.boxSizing Css.borderBox
        , Css.display Css.table
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


styledDock : Settings -> List (Html msg) -> Html msg
styledDock settings =
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.relative
        , case settings.dockOrientation of
            Horizontal ->
                Css.batch
                    [ Css.flex3 Css.zero Css.zero (Css.px (toFloat settings.dockHeight))
                    , Css.borderTop3 (Css.px 2) Css.solid Palette.smoke
                    ]

            Vertical ->
                Css.batch
                    [ Css.flex3 Css.zero Css.zero (Css.px (toFloat settings.dockWidth))
                    , Css.borderLeft3 (Css.px 2) Css.solid Palette.smoke
                    ]
        ]
        []


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
        [ Attributes.title (ifelse showGrid "Hide background grid" "Show background grid")
        ]
        [ Icon.grid
        ]


viewToggleBackground : Bool -> Html Msg
viewToggleBackground darkBackground =
    button
        { onPress = ToggleBackground
        , dark = darkBackground
        }
        [ Attributes.title (ifelse darkBackground "Set light background" "Set dark background")
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
        [ Attributes.title (ifelse addPaddings "Remove paddings" "Add paddings")
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
                    ( "Dock to right", Icon.dockHorizontal )

                Vertical ->
                    ( "Dock to bottom", Icon.dockVertical )
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


viewDock : Settings -> Html Msg -> Html Msg
viewDock settings knobs =
    styledDock settings
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


viewWorkspace : Story.Payload Renderer -> Settings -> State -> Addons -> Html Msg
viewWorkspace payload settings state addons =
    styledWorkspace settings.dockOrientation
        [ viewDragger Vertical
            [ Events.on "mousedown" (Decode.map StartNavigationResizing screenX)
            ]
        , case Result.map ((|>) addons) payload.view of
            Err error ->
                text error

            Ok (Renderer.Renderer layout) ->
                styledStoryScroller
                    [ styledStoryContainer settings
                        [ Html.map StoryMsg layout
                        ]
                    ]
        , Knob.view payload.knobs addons.knobs
            |> Html.map (KnobMsg state.current)
            |> viewDock settings
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


styledGlobal : Html msg
styledGlobal =
    global
        [ Css.Global.body
            [ Css.backgroundColor Palette.cloud
            ]
        , Css.Global.each
            [ Css.Global.html
            , Css.Global.body
            ]
            [ Css.minHeight (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        ]


viewRoot : Orientation -> Dragging -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewRoot dockOrientation dragging attributes children =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , case dragging of
            NoDragging ->
                Css.batch []

            NavigationResizing _ _ ->
                Css.batch
                    [ Css.property "user-select" "none"
                    , Css.cursor Css.ewResize
                    ]

            DockResizing _ _ ->
                Css.batch
                    [ Css.property "user-select" "none"
                    , case dockOrientation of
                        Horizontal ->
                            Css.cursor Css.nsResize

                        Vertical ->
                            Css.cursor Css.ewResize
                    ]
        ]
        attributes
        (styledGlobal :: children)


styledNavigation : Int -> List (Html msg) -> Html msg
styledNavigation size =
    styled nav
        [ Css.flex3 Css.zero Css.zero (Css.px (toFloat size))
        , Css.overflow Css.auto
        ]
        []


view : List (Story Renderer) -> Model -> Browser.Document Msg
view stories (Model settings state addons) =
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
            settings.dockOrientation
            state.dragging
            attrs
            [ styledNavigation settings.navigationWidth
                [ Html.map NavigationMsg (Navigation.view state.current stories state.navigation)
                ]
            , case Story.get state.current state.store of
                Nothing ->
                    viewEmpty

                Just payload ->
                    addons
                        |> Dict.get state.current
                        |> Maybe.withDefault Addons.initial
                        |> viewWorkspace payload settings state
            ]
            |> Html.toUnstyled
        ]


type alias Program =
    Platform.Program () Model Msg


run : List (Story Renderer) -> Program
run stories =
    Browser.application
        { init = init stories
        , update = update
        , view = view stories
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
