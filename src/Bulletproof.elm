module Bulletproof exposing
    ( Program
    , Renderer
    , Story
    , componentOf
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


extractFirstStoryID : List Story -> Maybe String
extractFirstStoryID stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case story of
                    Story.Story payload ->
                        Just payload.title

                    Story.Component _ substories ->
                        extractFirstStoryID substories

                    Story.Empty ->
                        Nothing
        )
        Nothing
        stories


init : List Story -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init stories () url navigationKey =
    let
        ( initialStoryID, initialCmd ) =
            case Router.parse url of
                Router.ToHome ->
                    ( Nothing
                    , extractFirstStoryID stories
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
viewItem currentID story =
    case story of
        Story.Story { title } ->
            div
                [ style "background" (ifelse (currentID == Just title) "#ccc" "#fff")
                ]
                [ link (Router.ToStory title)
                    []
                    [ text title
                    ]
                ]

        Story.Component title stories ->
            div
                []
                [ text title
                , div [] (List.map (viewItem currentID) stories)
                ]

        Story.Empty ->
            text ""


viewNavigation : Maybe String -> List (Story.Story a) -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : Addons -> Story.Payload Renderer -> Html Msg
viewStory addons payload =
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
        , Html.map KnobMsg (Knob.view payload.title payload.knobs addons.knobs)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


findCurrentStory : String -> List Story -> Maybe (Story.Payload Renderer)
findCurrentStory currentStoryID stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case story of
                    Story.Story payload ->
                        if currentStoryID == payload.title then
                            Just payload

                        else
                            Nothing

                    Story.Component _ substories ->
                        findCurrentStory currentStoryID substories

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

            Just currentStoryID ->
                case findCurrentStory currentStoryID stories of
                    Nothing ->
                        viewEmpty

                    Just payload ->
                        viewStory addons payload
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


componentOf : String -> List Story -> Story
componentOf title stories =
    Story.Component title stories


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
