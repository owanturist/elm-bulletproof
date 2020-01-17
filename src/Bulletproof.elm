module Bulletproof exposing (Program, program, storyOf)

import AVL.Dict as Dict exposing (Dict)
import Browser
import Browser.Navigation
import Html exposing (Html, div, nav, text)
import Html.Attributes exposing (style)
import Url exposing (Url)


ifelse : Bool -> x -> x -> x
ifelse bool onTrue onFalse =
    if bool then
        onTrue

    else
        onFalse



-- R O U T E R


type Route
    = ToHome
    | ToStory String



-- M O D E L


type alias Model =
    { current : Maybe String
    }


init : Maybe String -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init storyID () url navigationKey =
    ( Model storyID
    , Cmd.none
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StoryMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- V I E W


viewItem : Maybe String -> Story -> Html Msg
viewItem current story =
    div
        [ style "background" (ifelse (current == Just story.title) "#ccc" "fff")
        ]
        [ text story.title
        ]


viewNavigation : Maybe String -> List Story -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : Story -> Html Msg
viewStory story =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ story.view ()
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


view : List Story -> Model -> Browser.Document Msg
view stories model =
    Browser.Document "Bulletproof"
        [ viewNavigation model.current stories
        , case model.current of
            Nothing ->
                viewEmpty

            Just current ->
                stories
                    |> List.filter ((==) current << .title)
                    |> List.head
                    |> Maybe.map viewStory
                    |> Maybe.withDefault viewEmpty
        ]



-- A P I


type alias Story =
    { title : String
    , view : () -> Html Msg
    }


storyOf : String -> (() -> Html msg) -> Story
storyOf title story =
    Story title (Html.map (always StoryMsg) << story)


type alias Program =
    Platform.Program () Model Msg


program : List Story -> Program
program stories =
    Browser.application
        { init = init (Maybe.map .title <| List.head stories)
        , update = update
        , view = view stories
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
