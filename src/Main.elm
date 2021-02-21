module Main exposing (Program, run)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Button
import Dict exposing (Dict)
import Empty
import Error exposing (Error)
import Html exposing (Html, div, nav)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Lazy
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
import Settings exposing (Orientation(..), Settings, paddingSize)
import Story exposing (Story(..))
import Style
import SyntaxHighlight
import Task
import TextCode
import Url exposing (Url)
import Utils exposing (Viewport, ifelse, px)



-- M O D E L


type Dragging
    = NoDragging
    | NavigationResizing Int Int
    | DockResizing Int Int


isNoDragging : Dragging -> Bool
isNoDragging =
    (==) NoDragging


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


cssGlobal : Style.Sheet
cssGlobal =
    Style.batch
        [ Style.selector "body"
            [ Style.rule "background" Palette.cloud
            ]

        --
        , Style.selector "html, body"
            [ Style.rule "margin" "0"
            , Style.rule "padding" "0"
            , Style.rule "min-height" "100%"
            , Style.rule "height" "100%"
            ]

        --
        , Style.selector ("." ++ Style.className main__root__resizing__horizontal ++ " *")
            [ Style.rule "user-select" "none"
            , Style.rule "cursor" "ew-resize !important"
            ]

        --
        , Style.selector ("." ++ Style.className main__root__resizing__vertical ++ " *")
            [ Style.rule "user-select" "none"
            , Style.rule "cursor" "ns-resize !important"
            ]
        ]


css : Style.Sheet
css =
    Style.elements
        [ main__story_scroller
        , main__story_scroller__animated
        , main__story_container
        , main__story_container__with_paddings
        , main__story_container__dark_background
        , main__dragger
        , main__dragger__horizontal
        , main__dragger__vertical
        , main__dragger__high
        , main__dragger_line
        , main__dock
        , main__dock_scroller
        , main__workspace
        , main__workspace__horizontal
        , main__workspace__animated
        , main__menu
        , main__navigation
        , main__root
        ]


main__story_scroller : Style.Element
main__story_scroller =
    Style.el "main__story_scroller"
        [ Style.rule "display" "flex"
        , Style.rule "flex-direction" "column"
        , Style.rule "overflow" "auto"
        ]


main__story_scroller__animated : Style.Element
main__story_scroller__animated =
    [ Style.rule "transition" "width 150ms, height 150ms"
    ]
        |> Style.mod main__story_scroller "animated"


main__story_container : Style.Element
main__story_container =
    Style.el "main__story_container"
        [ Style.rule "all" "initial"
        , Style.rule "display" "block"
        , Style.rule "flex" "1 0 auto"
        , Style.rule "position" "relative"
        , Style.rule "background" Palette.white
        ]


main__story_container__with_paddings : Style.Element
main__story_container__with_paddings =
    [ Style.rule "padding" (px paddingSize)
    ]
        |> Style.mod main__story_container "with_paddings"


main__story_container__dark_background : Style.Element
main__story_container__dark_background =
    [ Style.rule "background" Palette.dark
    ]
        |> Style.mod main__story_container "dark_background"


main__dragger : Style.Element
main__dragger =
    Style.el "main__dragger"
        [ Style.rule "position" "absolute"
        , Style.rule "top" "0"
        , Style.rule "left" "0"
        , Style.rule "margin" "-2px"
        ]


main__dragger__horizontal : Style.Element
main__dragger__horizontal =
    [ Style.rule "right" "0"
    , Style.rule "height" "2px"
    , Style.rule "cursor" "ns-resize"
    , Style.rule "padding" "2px 0"
    ]
        |> Style.mod main__dragger "horizontal"


main__dragger__vertical : Style.Element
main__dragger__vertical =
    [ Style.rule "bottom" "0"
    , Style.rule "width" "2px"
    , Style.rule "cursor" "ew-resize"
    , Style.rule "padding" "0 2px"
    ]
        |> Style.mod main__dragger "vertical"


main__dragger__high : Style.Element
main__dragger__high =
    [ Style.rule "z-index" "2"
    , Style.rule "left" "auto"
    , Style.rule "right" "0"
    ]
        |> Style.mod main__dragger "high"


main__dragger_line : Style.Element
main__dragger_line =
    Style.el "main__dragger_line"
        [ Style.rule "height" "100%"
        , Style.rule "background" Palette.smoke
        ]


main__dock : Style.Element
main__dock =
    Style.el "main__dock"
        [ Style.rule "position" "relative"
        , Style.rule "flex" "1 1 0"
        , Style.rule "background" Palette.white
        , Style.rule "overflow" "hidden"
        ]


main__dock_scroller : Style.Element
main__dock_scroller =
    [ Style.rule "display" "flex"
    , Style.rule "width" "100%"
    , Style.rule "height" "100%"
    , Style.rule "overflow" "auto"
    ]
        |> Style.mod main__dock "scroller"


main__workspace : Style.Element
main__workspace =
    Style.el "main__workspace"
        [ Style.rule "position" "relative"
        , Style.rule "display" "flex"
        , Style.rule "flex-direction" "row"
        , Style.rule "box-shadow" ("0 0 10px " ++ Palette.smoke)
        ]


main__workspace__horizontal : Style.Element
main__workspace__horizontal =
    [ Style.rule "flex-direction" "column"
    ]
        |> Style.mod main__workspace "horizontal"


main__workspace__animated : Style.Element
main__workspace__animated =
    [ Style.rule "transition" "width 150ms"
    ]
        |> Style.mod main__workspace "animated"


main__menu : Style.Element
main__menu =
    Style.el "main__menu"
        [ Style.rule "position" "absolute"
        , Style.rule "z-index" "2"
        , Style.rule "top" "0"
        , Style.rule "left" "0"
        , Style.rule "margin" "12px"
        ]


main__navigation : Style.Element
main__navigation =
    Style.el "main__navigation"
        [ Style.rule "position" "relative"
        , Style.rule "flex" "1 1 0"
        , Style.rule "overflow" "hidden"
        ]


main__root : Style.Element
main__root =
    Style.el "main__root"
        [ Style.rule "position" "relative"
        , Style.rule "display" "flex"
        , Style.rule "flex-direction" "row"
        , Style.rule "flex-wrap" "nowrap"
        , Style.rule "width" "100%"
        , Style.rule "height" "100%"
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font
        ]


main__root__resizing__horizontal : Style.Element
main__root__resizing__horizontal =
    Style.mod main__root "horizontal" []


main__root__resizing__vertical : Style.Element
main__root__resizing__vertical =
    Style.mod main__root "vertical" []


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


cssGrid : Settings -> List (Html.Attribute msg)
cssGrid settings =
    let
        ( smallColor, bigColor ) =
            if settings.darkBackground then
                ( "#444", "#666" )

            else
                ( "#eee", "#ccc" )

        shift =
            ifelse settings.addPaddings paddingSize 0
    in
    [ Attributes.style "background-position" (px shift ++ " " ++ px shift)

    --
    , [ "100px 100px"
      , "100px 100px"
      , "10px 10px"
      , "10px 10px"
      ]
        |> String.join ","
        |> Attributes.style "background-size"

    --
    , [ linearGradient 0 bigColor
      , linearGradient 270 bigColor
      , linearGradient 0 smallColor
      , linearGradient 270 smallColor
      ]
        |> String.join ","
        |> Attributes.style "background-image"
    ]


viewDragger : List (Html.Attribute msg) -> Html msg
viewDragger attributes =
    div
        (Style.class main__dragger :: attributes)
        [ div [ Style.class main__dragger_line ] []
        ]


viewStoryScroller : Viewport -> Bool -> List (Html msg) -> Html msg
viewStoryScroller storyViewport noDragging =
    div
        [ Style.classList
            [ ( main__story_scroller, True )
            , ( main__story_scroller__animated, noDragging )
            ]
        , Attributes.style "width" (px storyViewport.width)
        , Attributes.style "height" (px storyViewport.height)
        ]


viewStoryContainer : Settings -> List (Html msg) -> Html msg
viewStoryContainer settings =
    div
        (Style.classList
            [ ( main__story_container, True )
            , ( main__story_container__with_paddings, settings.addPaddings )
            , ( main__story_container__dark_background, settings.darkBackground )
            ]
            :: ifelse settings.showGrid (cssGrid settings) []
        )


viewDock : Orientation -> Html Msg -> Html Msg
viewDock dockOrientation knobs =
    div
        [ Style.class main__dock
        ]
        [ viewDragger
            [ ifelse (Settings.isHorizontal dockOrientation)
                main__dragger__horizontal
                main__dragger__horizontal
                |> Style.class
            , Events.on "mousedown" (Decode.map2 StartDockResizing screenX screenY)
            ]
        , div [ Style.class main__dock_scroller ] [ knobs ]
        ]


styledWorkspace : Settings -> State -> List (Html msg) -> Html msg
styledWorkspace settings { viewport, dragging } =
    let
        width =
            viewport.width - ifelse settings.navigationVisible settings.navigationWidth 0
    in
    div
        [ Style.classList
            [ ( main__workspace, True )
            , ( main__workspace__horizontal, Settings.isHorizontal settings.dockOrientation )
            , ( main__workspace__animated, isNoDragging dragging )
            ]
        , Attributes.style "width" (px width)
        ]


viewStoryWorkspace : Int -> Int -> Knob.State -> (Knob.Payload -> Maybe (Html ())) -> Html ()
viewStoryWorkspace width height knobs workspaceView =
    Knob.Payload knobs (Viewport width height)
        |> workspaceView
        |> Maybe.withDefault (Html.text "")


viewStoryKnobs : Int -> Int -> Knob.State -> List ( String, Knob.Knob ) -> Html Knob.Msg
viewStoryKnobs width height knobs workspaceKnobs =
    Knob.view (Viewport width height) knobs workspaceKnobs


viewWorkspace : Story.Workspace (Html ()) -> Settings -> State -> Knob.State -> Html Msg
viewWorkspace workspace settings state knobs =
    let
        viewport =
            calcViewport settings state.viewport

        { width, height } =
            reducePaddingsForViewport settings.addPaddings viewport
    in
    styledWorkspace
        settings
        state
        [ viewStoryScroller
            viewport
            (isNoDragging state.dragging)
            [ viewStoryContainer settings
                [ Lazy.lazy4 viewStoryWorkspace width height knobs workspace.view
                    |> Html.map (always NoOp)
                ]
            ]

        --
        , Lazy.lazy4 viewStoryKnobs width height knobs workspace.knobs
            |> Html.map (KnobMsg state.current)
            |> viewDock settings.dockOrientation
        ]


viewStyle : Html msg
viewStyle =
    [ cssGlobal
    , Range.css
    , Button.css
    , Empty.css
    , Error.css
    , Icon.css
    , Knob.css
    , Menu.css
    , TextCode.css
    , Navigation.css
    , NotFound.css
    , css
    ]
        |> Style.batch
        |> Style.render
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


viewRoot : Orientation -> Dragging -> List (Html Msg) -> Html Msg
viewRoot dockOrientation dragging children =
    let
        attributes =
            case dragging of
                NoDragging ->
                    []

                NavigationResizing _ _ ->
                    [ Style.class main__root__resizing__horizontal
                    , Events.on "mousemove" (Decode.map Drag screenX)
                    , Events.on "mouseup" (Decode.succeed DragEnd)
                    , Events.on "mouseleave" (Decode.succeed DragEnd)
                    ]

                DockResizing _ _ ->
                    let
                        ( classSelector, positionDecoder ) =
                            if Settings.isHorizontal dockOrientation then
                                ( main__root__resizing__vertical, screenY )

                            else
                                ( main__root__resizing__horizontal, screenX )
                    in
                    [ Style.class classSelector
                    , Events.on "mousemove" (Decode.map Drag positionDecoder)
                    , Events.on "mouseup" (Decode.succeed DragEnd)
                    , Events.on "mouseleave" (Decode.succeed DragEnd)
                    ]
    in
    div
        (Style.class main__root :: attributes)
        (viewStyle :: SyntaxHighlight.useTheme SyntaxHighlight.gitHub :: children)


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
                        |> Lazy.lazy4 viewWorkspace workspace settings state
                    )
    in
    Browser.Document ("Bulletproof | " ++ String.join " / " state.current)
        [ viewRoot
            actualSettings.dockOrientation
            state.dragging
            [ div
                [ Style.class main__menu
                ]
                [ Lazy.lazy2 Menu.view
                    state.menuOpened
                    actualSettings
                    |> Html.map (MenuMsg story)
                ]

            --
            , nav
                [ Style.class main__navigation
                ]
                [ viewDragger
                    [ Style.class main__dragger__vertical
                    , Style.class main__dragger__high
                    , Events.on "mousedown" (Decode.map StartNavigationResizing screenX)
                    ]
                , Lazy.lazy3 Navigation.view
                    state.current
                    story
                    state.navigation
                    |> Html.map NavigationMsg
                ]

            --
            , workspaceView
            ]
        ]


viewError : List Error -> Model -> Browser.Document Msg
viewError errors { settings, state } =
    Browser.Document "Bulletproof | Error"
        [ viewRoot settings.dockOrientation state.dragging [ Error.view errors ]
        ]


viewEmpty : Model -> Browser.Document Msg
viewEmpty { settings, state } =
    Browser.Document "Bulletproof"
        [ viewRoot settings.dockOrientation state.dragging [ Empty.view ]
        ]


document : Story (Html ()) -> Model -> Browser.Document Msg
document story model =
    let
        errors =
            Error.validateStories story
    in
    if Story.isEmpty story then
        viewEmpty model

    else if List.isEmpty errors then
        viewBulletproof story model

    else
        viewError errors model


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
