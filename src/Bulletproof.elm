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
import Icon
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


type alias State =
    { key : Browser.Navigation.Key
    , current : List String
    , navigation : Navigation.Model
    , dock : ( Orientation, Int )
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
        , navigation = Navigation.open initialFolderPath Navigation.initial
        , dock = ( Horizontal, 300 )
        }
    , initialCmd
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ToggleDockOrientation
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
            ( case Tuple.first state.dock of
                Horizontal ->
                    Model addons { state | dock = ( Vertical, 400 ) }

                Vertical ->
                    Model addons { state | dock = ( Horizontal, 300 ) }
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


styledStory : List (Html msg) -> Html msg
styledStory =
    styled div
        [ Css.all Css.initial
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
        , Css.overflow Css.auto
        ]
        []


styledDragger : Orientation -> List (Html.Styled.Attribute msg) -> Html msg
styledDragger orientation attributes =
    styled div
        [ Css.position Css.absolute
        , Css.backgroundColor (Css.hex "#f00")
        , case orientation of
            Horizontal ->
                Css.batch
                    [ Css.top (Css.px -5)
                    , Css.right Css.zero
                    , Css.left Css.zero
                    , Css.height (Css.px 5)
                    , Css.cursor Css.rowResize
                    ]

            Vertical ->
                Css.batch
                    [ Css.top Css.zero
                    , Css.bottom Css.zero
                    , Css.left (Css.px -5)
                    , Css.width (Css.px 5)
                    , Css.cursor Css.colResize
                    ]
        ]
        attributes
        []


styledDock : Int -> List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledDock size =
    styled div
        [ Css.position Css.relative
        , Css.flex3 Css.zero Css.zero (Css.px (toFloat size))
        , Css.borderTop3 (Css.px 1) Css.solid Palette.smoke
        ]


styledDockHeader : List (Html msg) -> Html msg
styledDockHeader =
    styled div
        [ Css.padding (Css.px 8)
        , Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
        ]
        []


styledDockBody : List (Html msg) -> Html msg
styledDockBody =
    styled div [] []


viewDock : Orientation -> Int -> Html Msg -> Html Msg
viewDock dockOrientation dockSize knobs =
    styledDock dockSize
        []
        [ styledDragger dockOrientation []
        , styledDockHeader
            [ button ToggleDockOrientation
                [ case dockOrientation of
                    Horizontal ->
                        Icon.dockVertical

                    Vertical ->
                        Icon.dockHorizontal
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


viewWorkspace : ( Orientation, Int ) -> List String -> Story.Payload Renderer -> Addons -> Html Msg
viewWorkspace ( dockOrientation, dockSize ) path payload addons =
    styledWorkspace dockOrientation
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
                |> Html.Styled.map (KnobMsg path)
                |> viewDock dockOrientation dockSize
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


styledRoot : List (Html msg) -> Html msg
styledRoot children =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.noWrap
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        ]
        []
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
        [ styledRoot
            [ styledNavigation
                [ Navigation.view state.current stories state.navigation
                ]
                |> Html.Styled.map NavigationMsg
            , case Story.find state.current stories of
                Nothing ->
                    viewEmpty

                Just payload ->
                    addons
                        |> Dict.get state.current
                        |> Maybe.withDefault Addons.initial
                        |> viewWorkspace state.dock state.current payload
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
