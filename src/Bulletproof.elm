module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , groupOf
    , html
    , program
    , storyOf
    )

import AVL.Dict as Dict exposing (Dict)
import Addons exposing (Addons)
import Browser
import Browser.Navigation
import Html exposing (Html, div, hr, nav, text)
import Html.Attributes exposing (style)
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
    , current : Maybe (List String)
    }


type Model
    = Model (Dict (List String) Addons) State


extractFirstStoryPath : List Story -> Maybe (List String)
extractFirstStoryPath stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case story of
                    Story.None ->
                        Nothing

                    Story.Single storyID _ ->
                        Just [ storyID ]

                    Story.Batch groupID substories ->
                        Maybe.map ((::) groupID) (extractFirstStoryPath substories)
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
        Dict.empty
        { navigationKey = navigationKey
        , current = initialStoryPath
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
                    Model addons { state | current = Just storyPath }

                Router.ToNotFound ->
                    Model addons { state | current = Nothing }
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


viewStoryLink : Bool -> List String -> String -> Html msg
viewStoryLink active parentPath storyID =
    div
        [ style "background" (ifelse active "#ccc" "#fff")
        ]
        [ link (Router.ToStory (List.reverse (storyID :: parentPath)))
            []
            [ text storyID
            ]
        ]


viewStoryGroup : List String -> String -> Maybe (List String) -> List Story -> Html Msg
viewStoryGroup parentPath groupID currentStoryPath stories =
    div
        []
        [ text groupID
        , div [] (List.map (viewItem (groupID :: parentPath) currentStoryPath) stories)
        ]


viewItem : List String -> Maybe (List String) -> Story -> Html Msg
viewItem parentPath currentStoryPath story =
    case ( currentStoryPath, story ) of
        ( Nothing, Story.Single storyID _ ) ->
            viewStoryLink False parentPath storyID

        ( Just (fragmentID :: []), Story.Single storyID _ ) ->
            viewStoryLink (fragmentID == storyID) parentPath storyID

        ( Just (fragmentID :: _), Story.Single storyID _ ) ->
            viewStoryLink False parentPath storyID

        ( Nothing, Story.Batch groupID stories ) ->
            viewStoryGroup parentPath groupID Nothing stories

        ( Just (fragmentID :: restPath), Story.Batch groupID stories ) ->
            if fragmentID == groupID then
                viewStoryGroup parentPath groupID (Just restPath) stories

            else
                viewStoryGroup parentPath groupID Nothing stories

        _ ->
            text ""


viewNavigation : Maybe (List String) -> List Story -> Html Msg
viewNavigation currentStoryPath =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem [] currentStoryPath)


viewStory : List String -> Story.Payload Renderer -> Addons -> Html Msg
viewStory path payload addons =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ case payload.view of
            Err error ->
                text error

            Ok renderer ->
                let
                    (Renderer.Renderer layout) =
                        renderer addons
                in
                Html.map StoryMsg layout
        , hr [] []
        , Html.map (KnobMsg path) (Knob.view payload.knobs addons.knobs)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


findCurrentStory : List String -> List Story -> Maybe (Story.Payload Renderer)
findCurrentStory currentStoryPath stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case ( currentStoryPath, story ) of
                    ( fragmentID :: [], Story.Single storyID payload ) ->
                        if fragmentID == storyID then
                            Just payload

                        else
                            Nothing

                    ( fragmentID :: restPath, Story.Batch groupID substories ) ->
                        if fragmentID == groupID then
                            findCurrentStory restPath substories

                        else
                            Nothing

                    _ ->
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
                        addons
                            |> Dict.get currentStoryPath
                            |> Maybe.withDefault Addons.initial
                            |> viewStory currentStoryPath payload
        ]



-- A P I


type alias Renderer =
    Renderer.Renderer


html : Html msg -> Renderer
html layout =
    Renderer.Renderer (Html.map (always ()) layout)


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
