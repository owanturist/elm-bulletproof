module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , folderOf
    , fromElmCss
    , fromHtml
    , program
    , storyOf
    )

import AVL.Dict as Dict exposing (Dict)
import Addons exposing (Addons)
import Browser
import Browser.Navigation
import Css
import Css.Global exposing (global)
import Html
import Html.Styled exposing (Html, div, hr, nav, styled, text)
import Knob
import Navigation
import Palette
import Renderer
import Router
import Story exposing (Story)
import Url exposing (Url)



-- M O D E L


type alias State =
    { key : Browser.Navigation.Key
    , current : List String
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
    in
    ( Model
        Dict.empty
        { key = key
        , current = initialStoryPath
        , navigation = Navigation.open initialStoryPath Navigation.initial
        }
    , initialCmd
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | NavigationMsg Navigation.Msg
    | StoryMsg ()
    | KnobMsg (List String) Knob.Msg


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

        NavigationMsg navigationMsg ->
            ( Model addons { state | navigation = Navigation.update navigationMsg state.navigation }
            , Cmd.none
            )

        StoryMsg () ->
            ( Model addons state, Cmd.none )

        KnobMsg path knobMsg ->
            let
                storyAddons =
                    Maybe.withDefault Addons.initial (Dict.get path addons)

                nextStoryAddons =
                    { storyAddons | knobs = Knob.update knobMsg storyAddons.knobs }
            in
            ( Model (Dict.insert path nextStoryAddons addons) state
            , Cmd.none
            )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- V I E W


styledStory : List (Html msg) -> Html msg
styledStory children =
    styled div
        [ Css.float Css.left
        , Css.width (Css.pct 70)
        ]
        []
        children


viewStory : List String -> Story.Payload Renderer -> Addons -> Html Msg
viewStory path payload addons =
    styledStory
        [ case payload.view of
            Err error ->
                text error

            Ok renderer ->
                let
                    (Renderer.Renderer layout) =
                        renderer addons
                in
                Html.Styled.map StoryMsg layout
        , hr [] []
        , Html.Styled.map (KnobMsg path) (Knob.view payload.knobs addons.knobs)
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
        ]


styledRoot : List (Html msg) -> Html msg
styledRoot children =
    styled div
        []
        []
        (styledGlobal :: children)


styledNavigation : List (Html msg) -> Html msg
styledNavigation =
    styled nav
        [ Css.float Css.left
        , Css.width (Css.pct 30)
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
                        |> viewStory state.current payload
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
