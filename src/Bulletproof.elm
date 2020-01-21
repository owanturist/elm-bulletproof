module Bulletproof exposing
    ( Program
    , Renderer
    , Story
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
import Router
import Story exposing (Story)
import Url exposing (Url)
import Utils exposing (ifelse)



-- M O D E L


type alias State =
    { navigationKey : Browser.Navigation.Key
    , current : Maybe String
    }


type Model
    = Model Addons State


init : Maybe String -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init firstStoryID () url navigationKey =
    let
        ( initialStoryID, initialCmd ) =
            case Router.parse url of
                Router.ToHome ->
                    ( firstStoryID
                    , firstStoryID
                        |> Maybe.map (Router.replace navigationKey << Router.ToStory)
                        |> Maybe.withDefault Cmd.none
                    )

                Router.ToStory storyID ->
                    ( Just storyID, Cmd.none )
    in
    ( Model
        Addons.initial
        { navigationKey = navigationKey
        , current = initialStoryID
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
                Router.ToHome ->
                    Model addons { state | current = Nothing }

                Router.ToStory storyID ->
                    Model addons { state | current = Just storyID }
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


viewItem : Maybe String -> Story.Story a -> Html Msg
viewItem currentID (Story.Story story) =
    div
        [ style "background" (ifelse (currentID == Just story.title) "#ccc" "#fff")
        ]
        [ link (Router.ToStory story.title)
            []
            [ text story.title
            ]
        ]


viewNavigation : Maybe String -> List (Story.Story a) -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : Addons -> Story.Story Renderer -> Html Msg
viewStory addons (Story.Story story) =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ case story.view of
            Err error ->
                text error

            Ok renderer ->
                let
                    (Renderer layout) =
                        renderer addons
                in
                layout
        , hr [] []
        , Html.map KnobMsg (Knob.view story.title story.knobs addons.knobs)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


view : List Story -> Model -> Browser.Document Msg
view stories (Model addons state) =
    Browser.Document "Bulletproof"
        [ viewNavigation state.current stories
        , case state.current of
            Nothing ->
                viewEmpty

            Just current ->
                case List.filter (\(Story.Story story) -> story.title == current) stories of
                    [ currentStoryID ] ->
                        viewStory addons currentStoryID

                    _ ->
                        viewEmpty
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
    Story.Story
        { title = title
        , knobs = []
        , view = Ok (\_ -> view_)
        }


type alias Program =
    Platform.Program () Model Msg


program : List Story -> Program
program stories =
    let
        firstStoryID =
            case stories of
                [] ->
                    Nothing

                (Story.Story story) :: _ ->
                    Just story.title
    in
    Browser.application
        { init = init firstStoryID
        , update = update
        , view = view stories
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
