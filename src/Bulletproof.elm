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
import Browser.Navigation
import Button exposing (button)
import Css
import Css.Global exposing (global)
import Html
import Html.Styled exposing (Html, div, nav, styled, text)
import Html.Styled.Events as Events
import Icon
import Json.Decode as Decode exposing (Decoder)
import Knob
import Navigation
import Palette
import Renderer
import Router
import Story exposing (Story)
import Url exposing (Url)



-- M O D E L


type Orientation
    = Horizontal
    | Vertical


type Dragging
    = NoDragging
    | DockResizing Int Int Int


type alias State =
    { key : Browser.Navigation.Key
    , current : List String
    , dragging : Dragging
    , dockOrientation : Orientation
    , dockSize : Int
    , navigation : Navigation.Model
    }


type Model
    = Model (Dict (List String) Addons) State


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
        Dict.empty
        { key = key
        , current = initialStoryPath
        , dragging = NoDragging
        , dockOrientation = Horizontal
        , dockSize = 300
        , navigation = Navigation.open initialFolderPath Navigation.initial
        }
    , initialCmd
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ToggleDockOrientation
    | StartDockResizing Int Int
    | Drag Int
    | DragEnd
    | NavigationMsg Navigation.Msg
    | KnobMsg (List String) Knob.Msg
    | StoryMsg ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model addons state) =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( Model addons state
            , Browser.Navigation.pushUrl state.key (Url.toString url)
            )

        UrlRequested (Browser.External path) ->
            ( Model addons state
            , Browser.Navigation.load path
            )

        UrlChanged url ->
            ( case Router.parse url of
                Router.ToStory storyPath ->
                    Model addons { state | current = storyPath }

                Router.ToNotFound ->
                    Model addons { state | current = [] }
            , Cmd.none
            )

        ToggleDockOrientation ->
            ( case state.dockOrientation of
                Horizontal ->
                    Model addons
                        { state
                            | dockOrientation = Vertical
                            , dockSize = 400
                        }

                Vertical ->
                    Model addons
                        { state
                            | dockOrientation = Horizontal
                            , dockSize = 300
                        }
            , Cmd.none
            )

        StartDockResizing maximum start ->
            ( Model addons { state | dragging = DockResizing maximum start start }
            , Cmd.none
            )

        Drag end ->
            ( case state.dragging of
                NoDragging ->
                    Model addons state

                DockResizing maximum start _ ->
                    Model addons { state | dragging = DockResizing maximum start end }
            , Cmd.none
            )

        DragEnd ->
            ( case state.dragging of
                NoDragging ->
                    Model addons state

                DockResizing maximum start end ->
                    Model addons
                        { state
                            | dragging = NoDragging
                            , dockSize = clamp 150 (maximum - 150) (state.dockSize + start - end)
                        }
            , Cmd.none
            )

        NavigationMsg msgOfNavigation ->
            let
                ( nextNavigation, cmdOfNavigation ) =
                    Navigation.update state.key msgOfNavigation state.navigation
            in
            ( Model addons { state | navigation = nextNavigation }
            , Cmd.map NavigationMsg cmdOfNavigation
            )

        KnobMsg path msgOfKnob ->
            let
                storyAddons =
                    Maybe.withDefault Addons.initial (Dict.get path addons)

                nextStoryAddons =
                    { storyAddons | knobs = Knob.update msgOfKnob storyAddons.knobs }
            in
            ( Model (Dict.insert path nextStoryAddons addons) state
            , Cmd.none
            )

        StoryMsg () ->
            ( Model addons state, Cmd.none )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- V I E W


screenX : Decoder Int
screenX =
    Decode.field "screenX" Decode.int


screenY : Decoder Int
screenY =
    Decode.field "screenY" Decode.int


grandParentWidth : Decoder Int
grandParentWidth =
    Decode.at [ "target", "parentElement", "parentElement", "offsetWidth" ] Decode.int


grandParentHeight : Decoder Int
grandParentHeight =
    Decode.at [ "target", "parentElement", "parentElement", "offsetHeight" ] Decode.int


styledStory : List (Html msg) -> Html msg
styledStory =
    styled div
        [ Css.all Css.initial
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.overflow Css.auto
        , Css.cursor Css.inherit
        ]
        []


styledDragger : Orientation -> List (Html.Styled.Attribute msg) -> Html msg
styledDragger orientation attributes =
    styled div
        [ Css.position Css.absolute
        , case orientation of
            Horizontal ->
                Css.batch
                    [ Css.top (Css.px -3)
                    , Css.right Css.zero
                    , Css.left Css.zero
                    , Css.height (Css.px 4)
                    , Css.cursor Css.rowResize
                    ]

            Vertical ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.bottom Css.zero
                    , Css.left (Css.px -3)
                    , Css.width (Css.px 4)
                    , Css.cursor Css.colResize
                    ]
        ]
        attributes
        []


styledDock : Orientation -> Int -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledDock orientation size =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.position Css.relative
        , Css.flex3 Css.zero Css.zero (Css.px (toFloat size))
        , case orientation of
            Horizontal ->
                Css.borderTop3 (Css.px 2) Css.solid Palette.smoke

            Vertical ->
                Css.borderLeft3 (Css.px 2) Css.solid Palette.smoke
        ]


styledDockHeader : List (Html msg) -> Html msg
styledDockHeader =
    styled div
        [ Css.padding2 (Css.px 8) (Css.px 12)
        , Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
        ]
        []


styledDockBody : List (Html msg) -> Html msg
styledDockBody =
    styled div
        [ Css.overflow Css.auto
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.padding2 (Css.px 8) (Css.px 12)
        ]
        []


viewDock : Orientation -> Int -> Html Msg -> Html Msg
viewDock dockOrientation dockSize knobs =
    let
        ( grandParentSizeDecoder, startPointDecoder, orientationIcon ) =
            case dockOrientation of
                Horizontal ->
                    ( grandParentHeight, screenY, Icon.dockVertical )

                Vertical ->
                    ( grandParentWidth, screenX, Icon.dockHorizontal )
    in
    styledDock
        dockOrientation
        dockSize
        []
        [ styledDragger dockOrientation
            [ Events.on "mousedown" (Decode.map2 StartDockResizing grandParentSizeDecoder startPointDecoder)
            ]
        , styledDockHeader
            [ button ToggleDockOrientation
                [ orientationIcon
                ]
            ]
        , styledDockBody
            [ knobs
            ]
        ]


styledWorkspace : Orientation -> List (Html msg) -> Html msg
styledWorkspace dockOrientation =
    styled div
        [ Css.displayFlex
        , case dockOrientation of
            Horizontal ->
                Css.flexDirection Css.column

            Vertical ->
                Css.flexDirection Css.row
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.backgroundColor Palette.white
        ]
        []


viewWorkspace : Story.Payload Renderer -> State -> Addons -> Html Msg
viewWorkspace payload state addons =
    let
        dockSize =
            case state.dragging of
                DockResizing maximum start end ->
                    clamp 150 (maximum - 150) (state.dockSize + start - end)

                _ ->
                    state.dockSize
    in
    styledWorkspace state.dockOrientation
        [ case Result.map ((|>) addons) payload.view of
            Err error ->
                text error

            Ok (Renderer.Renderer layout) ->
                styledStory
                    [ Html.Styled.map StoryMsg layout
                    ]
        , if List.isEmpty payload.knobs then
            text ""

          else
            Knob.view payload.knobs addons.knobs
                |> Html.Styled.map (KnobMsg state.current)
                |> viewDock state.dockOrientation dockSize
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


styledRoot : State -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledRoot state attributes children =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , case state.dragging of
            NoDragging ->
                Css.batch []

            DockResizing _ _ _ ->
                Css.batch
                    [ Css.property "user-select" "none"
                    , case state.dockOrientation of
                        Horizontal ->
                            Css.cursor Css.rowResize

                        Vertical ->
                            Css.cursor Css.colResize
                    ]
        ]
        attributes
        (styledGlobal :: children)


styledNavigation : List (Html msg) -> Html msg
styledNavigation =
    styled nav
        [ Css.flex3 Css.zero Css.zero (Css.px 240)
        , Css.overflow Css.auto
        ]
        []


view : List Story -> Model -> Browser.Document Msg
view stories (Model addons state) =
    Browser.Document "Bulletproof"
        [ styledRoot state
            (case state.dragging of
                NoDragging ->
                    []

                DockResizing _ _ _ ->
                    [ case state.dockOrientation of
                        Horizontal ->
                            Events.on "mousemove" (Decode.map Drag screenY)

                        Vertical ->
                            Events.on "mousemove" (Decode.map Drag screenX)
                    , Events.on "mouseup" (Decode.succeed DragEnd)
                    , Events.on "mouseleave" (Decode.succeed DragEnd)
                    ]
            )
            [ styledNavigation
                [ Html.Styled.map NavigationMsg (Navigation.view state.current stories state.navigation)
                ]
            , case Story.find state.current stories of
                Nothing ->
                    viewEmpty

                Just payload ->
                    addons
                        |> Dict.get state.current
                        |> Maybe.withDefault Addons.initial
                        |> viewWorkspace payload state
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
