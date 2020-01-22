module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , groupOf
    , html
    , program
    , storyOf
    )

import Addons exposing (Addons)
import Browser
import Browser.Navigation
import Html exposing (Html, div, hr, nav, text)
import Html.Attributes exposing (style)
import Knob
import Link exposing (link)
import Path exposing (Path)
import Router
import Story exposing (Story)
import Url exposing (Url)
import Utils exposing (ifelse)



-- M O D E L


type alias State =
    { navigationKey : Browser.Navigation.Key
    , current : Maybe Path
    }


type Model
    = Model Addons State


extractFirstStoryPath : List Story -> Maybe Path
extractFirstStoryPath stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case story of
                    Story.Story path _ ->
                        Just path

                    Story.Group _ substories ->
                        extractFirstStoryPath substories

                    Story.Empty ->
                        Nothing
        )
        Nothing
        stories


init : List Story -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories () url navigationKey =
    let
        ( initialStoryPath, initialCmd ) =
            case Router.parse url of
                Router.ToStory storyPath ->
                    ( Just storyPath, Cmd.none )

                Router.ToNotFound ->
                    ( Nothing
                    , extractFirstStoryPath stories
                        |> Maybe.map (Router.replace navigationKey << Router.ToStory)
                        |> Maybe.withDefault Cmd.none
                    )
    in
    ( Model
        Addons.initial
        { navigationKey = navigationKey
        , current = initialStoryPath
        }
    , initialCmd
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StoryMsg
    | KnobMsg Knob.Msg


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
                    Model addons { state | current = Just storyPath }

                Router.ToNotFound ->
                    Model addons { state | current = Nothing }
            , Cmd.none
            )

        StoryMsg ->
            ( Model addons state, Cmd.none )

        KnobMsg knobMsg ->
            ( Model { addons | knobs = Knob.update knobMsg addons.knobs } state
            , Cmd.none
            )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- V I E W


viewItem : Maybe Path -> Story.Story a -> Html Msg
viewItem currentPath story =
    case story of
        Story.Story path _ ->
            div
                [ style "background" (ifelse (currentPath == Just path) "#ccc" "#fff")
                ]
                [ link (Router.ToStory path)
                    []
                    [ text (Path.toStoryTitle path)
                    ]
                ]

        Story.Group title stories ->
            div
                []
                [ text title
                , div [] (List.map (viewItem currentPath) stories)
                ]

        _ ->
            text ""


viewNavigation : Maybe Path -> List (Story.Story a) -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : Addons -> Path -> Story.Payload Renderer -> Html Msg
viewStory addons path payload =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ case payload.view of
            Err error ->
                text error

            Ok renderer ->
                let
                    (Renderer layout) =
                        renderer addons
                in
                layout
        , hr [] []
        , Html.map KnobMsg (Knob.view path payload.knobs addons.knobs)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


findCurrentStory : Path -> List Story -> Maybe (Story.Payload Renderer)
findCurrentStory currentStoryPath stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case story of
                    Story.Story path payload ->
                        if currentStoryPath == path then
                            Just payload

                        else
                            Nothing

                    Story.Group _ substories ->
                        findCurrentStory currentStoryPath substories

                    Story.Empty ->
                        Nothing
        )
        Nothing
        stories


view : List Story -> Model -> Browser.Document Msg
view stories (Model addons state) =
    Browser.Document "Bulletproof"
        [ viewNavigation state.current stories
        , case state.current of
            Nothing ->
                viewEmpty

            Just currentStoryPath ->
                case findCurrentStory currentStoryPath stories of
                    Nothing ->
                        viewEmpty

                    Just payload ->
                        viewStory addons currentStoryPath payload
        ]



-- A P I


type Renderer
    = Renderer (Html Msg)


html : Html msg -> Renderer
html layout =
    Renderer (Html.map (always StoryMsg) layout)


type alias Story =
    Story.Story Renderer


storyOf : String -> view -> Story.Story view
storyOf title view_ =
    Story.Story (Path.Alone title)
        { knobs = []
        , view = Ok (\_ -> view_)
        }


groupOf : String -> List Story -> Story
groupOf title stories =
    Story.Group title stories


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
