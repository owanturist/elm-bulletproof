module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , css
    , groupOf
    , html
    , program
    , storyOf
    )

import AVL.Dict as Dict exposing (Dict)
import Addons exposing (Addons)
import Browser
import Browser.Navigation
import Css
import Html
import Html.Styled exposing (Html, div, hr, nav, styled, text)
import Knob
import Link exposing (link)
import Renderer
import Router
import Story exposing (Story)
import Url exposing (Url)
import Utils exposing (ifelse)



-- M O D E L


type alias State =
    { navigationKey : Browser.Navigation.Key
    , current : List String
    }


type Model
    = Model (Dict (List String) Addons) State


init : List Story -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories () url navigationKey =
    let
        ( initialStoryPath, initialCmd ) =
            case Router.parse url of
                Router.ToStory storyPath ->
                    ( Just storyPath, Cmd.none )

                Router.ToNotFound ->
                    ( Nothing
                    , Story.firstPath stories
                        |> Router.ToStory
                        |> Router.replace navigationKey
                    )
    in
    ( Model
        Dict.empty
        { navigationKey = navigationKey
        , current = Maybe.withDefault [] initialStoryPath
        }
    , initialCmd
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StoryMsg ()
    | KnobMsg (List String) Knob.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model addons state) =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( Model addons state
            , Browser.Navigation.pushUrl state.navigationKey (Url.toString url)
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


styledNavigationItem : Bool -> List (Html msg) -> Html msg
styledNavigationItem active children =
    styled div
        [ Css.backgroundColor (Css.hex (ifelse active "#ccc" "#fff"))
        ]
        []
        children


viewNavigationStory : Bool -> List String -> String -> Html msg
viewNavigationStory active parentPath storyID =
    styledNavigationItem active
        [ link (Router.ToStory (List.reverse (storyID :: parentPath)))
            []
            [ text storyID
            ]
        ]


viewStoryGroup : List String -> String -> List String -> List Story -> Html Msg
viewStoryGroup parentPath groupID currentStoryPath stories =
    div
        []
        [ text groupID
        , div [] (List.map (viewItem (groupID :: parentPath) currentStoryPath) stories)
        ]


viewItem : List String -> List String -> Story -> Html Msg
viewItem parentPath currentStoryPath story =
    case ( currentStoryPath, story ) of
        ( fragmentID :: [], Story.Single storyID _ ) ->
            viewNavigationStory (fragmentID == storyID) parentPath storyID

        ( _, Story.Single storyID _ ) ->
            viewNavigationStory False parentPath storyID

        ( [], Story.Batch groupID stories ) ->
            viewStoryGroup parentPath groupID [] stories

        ( fragmentID :: restPath, Story.Batch groupID stories ) ->
            if fragmentID == groupID then
                viewStoryGroup parentPath groupID restPath stories

            else
                viewStoryGroup parentPath groupID [] stories


styledNavigationPanel : List (Html msg) -> Html msg
styledNavigationPanel children =
    styled nav
        [ Css.float Css.left
        , Css.width (Css.pct 30)
        ]
        []
        children


viewNavigation : List String -> List Story -> Html Msg
viewNavigation currentStoryPath stories =
    styledNavigationPanel (List.map (viewItem [] currentStoryPath) stories)


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


styledRoot : List (Html msg) -> Html msg
styledRoot children =
    styled div
        []
        []
        children


view : List Story -> Model -> Browser.Document Msg
view stories (Model addons state) =
    Browser.Document "Bulletproof"
        [ styledRoot
            [ viewNavigation state.current stories
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


html : Html.Html msg -> Renderer
html layout =
    Renderer.Renderer (Html.Styled.map (always ()) (Html.Styled.fromUnstyled layout))


css : Html.Styled.Html msg -> Renderer
css layout =
    Renderer.Renderer (Html.Styled.map (always ()) layout)


type alias Story =
    Story.Story Renderer


storyOf : String -> view -> Story.Story view
storyOf title view_ =
    Story.Single title
        { knobs = []
        , view = Ok (\_ -> view_)
        }


groupOf : String -> List Story -> Story
groupOf title stories =
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
