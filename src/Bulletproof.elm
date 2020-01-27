module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , folderOf
    , fromElmCss
    , fromHtml
    , label
    , program
    , storyOf
    )

import AVL.Dict as Dict exposing (Dict)
import Addons exposing (Addons)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Button exposing (button)
import Css
import Css.Global exposing (global)
import Html
import Html.Styled exposing (Html, div, nav, styled, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Icon
import Json.Decode as Decode exposing (Decoder)
import Knob
import Navigation
import Palette
import Renderer
import Router
import Story exposing (Story)
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
    }


defaultSettings : Settings
defaultSettings =
    { navigationWidth = 200
    , dockWidth = 400
    , dockHeight = 300
    , dockOrientation = Horizontal
    , addPaddings = False
    , darkBackground = False
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
    , current : List String
    , dragging : Dragging
    , navigation : Navigation.Model
    }


type Model
    = Model Settings State (Dict (List String) Addons)


init : List Story -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories () url key =
    let
        ( initialStoryPath, initialCmd ) =
            case Router.parse url of
                Router.ToStory storyPath ->
                    ( storyPath, Cmd.none )

                Router.ToNotFound ->
                    ( []
                    , Story.firstPath stories
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
    | TogglePaddings
    | ToggleBackground
    | StartNavigationResizing Int
    | StartDockResizing Int Int
    | Drag Int
    | DragEnd
    | NavigationMsg Navigation.Msg
    | KnobMsg (List String) Knob.Msg
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
    Browser.Events.onResize ViewportChanged



-- V I E W


screenX : Decoder Int
screenX =
    Decode.field "screenX" Decode.int


screenY : Decoder Int
screenY =
    Decode.field "screenY" Decode.int


styledStory : Settings -> List (Html msg) -> Html msg
styledStory settings =
    styled div
        [ Css.all Css.initial
        , Css.position Css.relative
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.border3 (Css.px (ifelse settings.addPaddings 12 0)) Css.solid Css.transparent
        , Css.backgroundColor (ifelse settings.darkBackground Palette.dark Palette.white)
        , Css.overflow Css.auto
        , Css.cursor Css.inherit
        ]
        []


viewDragger : Orientation -> List (Html.Styled.Attribute msg) -> Html msg
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


viewToggleBackground : Bool -> Html Msg
viewToggleBackground darkBackground =
    let
        ( title, icon ) =
            if darkBackground then
                ( "Set light background", Icon.fillDrop )

            else
                ( "Set dark background", Icon.fill )
    in
    button ToggleBackground
        [ Attributes.title title
        ]
        [ icon
        ]


viewTogglePaddings : Bool -> Html Msg
viewTogglePaddings addPaddings =
    let
        ( title, icon ) =
            if addPaddings then
                ( "Remove paddings", Icon.bordersBold )

            else
                ( "Add paddings", Icon.bordersThin )
    in
    button TogglePaddings
        [ Attributes.title title
        , Attributes.css cssNextButton
        ]
        [ icon
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
    button ToggleDockOrientation
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
            [ viewToggleBackground settings.darkBackground
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
                styledStory settings
                    [ Html.Styled.map StoryMsg layout
                    ]
        , Knob.view payload.knobs addons.knobs
            |> Html.Styled.map (KnobMsg state.current)
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


viewRoot : Orientation -> Dragging -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
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


view : List Story -> Model -> Browser.Document Msg
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
                [ Html.Styled.map NavigationMsg (Navigation.view state.current stories state.navigation)
                ]
            , case Story.find state.current stories of
                Nothing ->
                    viewEmpty

                Just payload ->
                    addons
                        |> Dict.get state.current
                        |> Maybe.withDefault Addons.initial
                        |> viewWorkspace payload settings state
            ]
            |> Html.Styled.toUnstyled
        ]



-- A P I


type alias Renderer =
    Renderer.Renderer


fromHtml : Html.Html msg -> Renderer
fromHtml layout =
    Renderer.Renderer (Html.Styled.map (always ()) (Html.Styled.fromUnstyled layout))


fromElmCss : Html.Styled.Html msg -> Renderer
fromElmCss layout =
    Renderer.Renderer (Html.Styled.map (always ()) layout)


type alias Story =
    Story.Story Renderer


storyOf : String -> view -> Story.Story view
storyOf title view_ =
    Story.Single title
        { knobs = []
        , view = Ok (\_ -> view_)
        }


folderOf : String -> List Story -> Story
folderOf title stories =
    Story.Batch title stories


label : String -> Story
label =
    Story.Label


type alias Program =
    Platform.Program () Model Msg


program : List Story -> Program
program stories =
    Browser.application
        { init = init stories
        , update = update
        , view = view stories
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
