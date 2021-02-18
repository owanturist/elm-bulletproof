module Main exposing (Program, run)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Button
import Css
import Css.Global exposing (global)
import Css.Transitions exposing (transition)
import Dict exposing (Dict)
import Empty
import Error exposing (Error)
import Html.Styled as Html exposing (Html, div, nav, styled)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Icon
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode exposing (encode)
import Knob
import Menu
import Navigation
import NotFound
import Palette
import Range
import Router
import Settings exposing (Orientation(..), Settings)
import Story exposing (Story(..))
import Style
import SyntaxHighlight
import Task
import Url exposing (Url)
import Utils exposing (Viewport, ifelse, px)


paddingSize : Int
paddingSize =
    10



-- M O D E L


type Dragging
    = NoDragging
    | NavigationResizing Int Int
    | DockResizing Int Int


type alias State =
    { key : Browser.Navigation.Key
    , viewport : Viewport
    , menuOpened : Bool
    , current : Story.Path
    , dragging : Dragging
    , navigation : Navigation.Model
    }


type alias Model =
    { settings : Settings
    , state : State
    , knobs : Dict Story.Path Knob.State
    }


init : Story (Html ()) -> Maybe String -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init story settingsJSON url key =
    let
        initialSettings =
            settingsJSON
                |> Maybe.andThen (Result.toMaybe << decodeString Settings.decoder)
                |> Maybe.withDefault Settings.default

        initialStoryPath =
            Router.parse url
    in
    ( Model
        initialSettings
        { key = key
        , viewport = Viewport 0 0
        , menuOpened = False
        , current = initialStoryPath
        , dragging = NoDragging
        , navigation = Navigation.open initialStoryPath Navigation.initial
        }
        Dict.empty
    , Cmd.batch
        [ if List.isEmpty initialStoryPath then
            case Story.getFirstPath story of
                Nothing ->
                    Cmd.none

                Just firstStoryPath ->
                    Router.replace key firstStoryPath

          else
            Cmd.none

        --
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
    | MenuMsg (Story (Html ())) Menu.Msg
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
            , if List.isEmpty (Router.parse url) then
                Cmd.none

              else
                Browser.Navigation.pushUrl state.key (Url.toString url)
            )

        UrlRequested (Browser.External path) ->
            ( Model settings state knobs
            , Browser.Navigation.load path
            )

        UrlChanged url ->
            ( case Router.parse url of
                [] ->
                    Model settings { state | current = [] } knobs

                storyPath ->
                    Model settings { state | current = storyPath } knobs
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

        MenuMsg story msgOfMenu ->
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
                    case Story.getPrevPath state.current story of
                        Nothing ->
                            ( Model settings state knobs
                            , Cmd.none
                            )

                        Just prevStoryPath ->
                            ( Model settings { state | navigation = Navigation.open prevStoryPath state.navigation } knobs
                            , Router.push state.key prevStoryPath
                            )

                Menu.NextStory ->
                    case Story.getNextPath state.current story of
                        Nothing ->
                            ( Model settings state knobs
                            , Cmd.none
                            )

                        Just nextStoryPath ->
                            ( Model settings { state | navigation = Navigation.open nextStoryPath state.navigation } knobs
                            , Router.push state.key nextStoryPath
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


subscriptions : Story (Html ()) -> Model -> Sub Msg
subscriptions story { state } =
    Sub.batch
        [ Browser.Events.onResize ViewportChanged
        , Sub.map (MenuMsg story) (Menu.subscriptions state.menuOpened)
        ]



-- V I E W


calcViewport : Settings -> Viewport -> Viewport
calcViewport settings { width, height } =
    case settings.dockOrientation of
        Horizontal ->
            Viewport
                (width - ifelse settings.navigationVisible settings.navigationWidth 0)
                (height - ifelse settings.dockVisible settings.dockHeight 0)

        Vertical ->
            Viewport
                (width
                    - ifelse settings.navigationVisible settings.navigationWidth 0
                    - ifelse settings.dockVisible settings.dockWidth 0
                )
                height


reducePaddingsForViewport : Bool -> Viewport -> Viewport
reducePaddingsForViewport addPaddings viewport =
    if addPaddings then
        Viewport
            (viewport.width - 2 * paddingSize)
            (viewport.height - 2 * paddingSize)

    else
        viewport


screenX : Decoder Int
screenX =
    Decode.field "screenX" Decode.int


screenY : Decoder Int
screenY =
    Decode.field "screenY" Decode.int


styledStoryScroller : Viewport -> Bool -> List (Html msg) -> Html msg
styledStoryScroller storyViewport isDragging =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.overflow Css.auto

        --
        , if isDragging then
            Css.batch []

          else
            transition
                [ Css.Transitions.width 150
                , Css.Transitions.height 150
                ]
        ]
        [ Attributes.style "width" (px storyViewport.width)
        , Attributes.style "height" (px storyViewport.height)
        ]


styledStoryContainer : Settings -> List (Html msg) -> Html msg
styledStoryContainer settings =
    styled div
        [ Css.all Css.initial
        , Css.flex2 (Css.int 1) Css.zero
        , Css.flexBasis Css.auto
        , Css.display Css.block
        , Css.position Css.relative
        , Css.padding (Css.px (ifelse settings.addPaddings (toFloat paddingSize) 0))
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
        , Css.margin (Css.px -2)
        , case orientation of
            Horizontal ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.right Css.zero
                    , Css.left Css.zero
                    , Css.height (Css.px 2)
                    , Css.cursor Css.nsResize
                    , Css.padding2 (Css.px 2) Css.zero
                    ]

            Vertical ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.bottom Css.zero
                    , Css.left Css.zero
                    , Css.width (Css.px 2)
                    , Css.cursor Css.ewResize
                    , Css.padding2 Css.zero (Css.px 2)
                    ]
        , Css.batch styles
        ]
        attributes
        [ styled div
            [ Css.backgroundColor Palette.smoke
            , Css.height (Css.pct 100)
            ]
            []
            []
        ]


styledDockScroller : List (Html msg) -> Html msg
styledDockScroller =
    styled div
        [ Css.displayFlex
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.overflow Css.auto
        ]
        []


styledDock : List (Html msg) -> Html msg
styledDock =
    styled div
        [ Css.position Css.relative
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.backgroundColor Palette.white
        , Css.overflow Css.hidden
        ]
        []


viewDock : Settings -> Html Msg -> Html Msg
viewDock settings knobs =
    styledDock
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
        [ Attributes.style "width" (px width)
        ]


viewWorkspace : Story.Workspace (Html ()) -> Settings -> State -> Knob.State -> Html Msg
viewWorkspace workspace settings state knobs =
    let
        viewport =
            calcViewport settings state.viewport

        storyViewport =
            reducePaddingsForViewport settings.addPaddings viewport
    in
    styledWorkspace
        settings
        state
        [ styledStoryScroller
            viewport
            (state.dragging /= NoDragging)
            [ styledStoryContainer settings
                [ Knob.Payload knobs storyViewport
                    |> workspace.view
                    |> Maybe.withDefault (Html.text "")
                    |> Html.map (always NoOp)
                ]
            ]

        --
        , Knob.view storyViewport knobs workspace.knobs
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


viewSyntaxHighlightStyle : Html msg
viewSyntaxHighlightStyle =
    Html.fromUnstyled (SyntaxHighlight.useTheme SyntaxHighlight.gitHub)


viewStyle : Html msg
viewStyle =
    [ Range.css
    , Button.css
    , Empty.css
    , Error.css
    , Icon.css
    , Knob.css
    ]
        |> Style.render
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


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
        (viewStyle :: styledGlobal settings dragging :: viewSyntaxHighlightStyle :: children)


viewBulletproof : Story (Html ()) -> Model -> Browser.Document Msg
viewBulletproof story { settings, state, knobs } =
    let
        ( actualSettings, workspaceView ) =
            case Story.getWorkspace state.current story of
                Nothing ->
                    ( { settings | navigationVisible = True }
                    , styledWorkspace
                        { settings | navigationVisible = True }
                        state
                        [ NotFound.view state.current
                        ]
                    )

                Just workspace ->
                    ( settings
                    , knobs
                        |> Dict.get state.current
                        |> Maybe.withDefault Knob.initial
                        |> viewWorkspace workspace settings state
                    )
    in
    Browser.Document ("Bulletproof | " ++ String.join " / " state.current)
        [ viewRoot
            actualSettings
            state.dragging
            [ styledMenu
                [ Html.map
                    (MenuMsg story)
                    (Menu.view state.menuOpened actualSettings)
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
                , Html.map
                    NavigationMsg
                    (Navigation.view state.current story state.navigation)
                ]

            --
            , workspaceView
            ]
            |> Html.toUnstyled
        ]


viewError : List Error -> Browser.Document Msg
viewError errors =
    Browser.Document "Bulletproof | Error"
        [ viewRoot Settings.default NoDragging [ Error.view errors ]
            |> Html.toUnstyled
        ]


viewEmpty : Browser.Document Msg
viewEmpty =
    Browser.Document "Bulletproof"
        [ viewRoot Settings.default NoDragging [ Empty.view ]
            |> Html.toUnstyled
        ]


document : Story (Html ()) -> Model -> Browser.Document Msg
document story model =
    let
        errors =
            Error.validateStories story
    in
    if Story.isEmpty story then
        viewEmpty

    else if List.isEmpty errors then
        viewBulletproof story model

    else
        viewError errors


type alias Program flags =
    Platform.Program flags Model Msg


run :
    { settingsFromFlags : flags -> Maybe String
    , onSettingsChange : String -> Cmd msg
    }
    -> Story (Html ())
    -> Program flags
run { settingsFromFlags, onSettingsChange } story =
    Browser.application
        { init = init story << settingsFromFlags
        , update = update onSettingsChange
        , view = document story
        , subscriptions = subscriptions story
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
