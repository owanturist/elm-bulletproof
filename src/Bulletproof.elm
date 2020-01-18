module Bulletproof exposing (Program, program, storyOf)

import Browser
import Browser.Navigation
import Html exposing (Html, a, div, nav, text)
import Html.Attributes exposing (style)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (Parser)


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


urlParser : Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map ToHome Url.Parser.top
        , Url.Parser.map ToStory Url.Parser.string
        ]


parseUrl : Url -> Route
parseUrl =
    Maybe.withDefault ToHome << Url.Parser.parse urlParser


toPath : Route -> String
toPath route =
    case route of
        ToHome ->
            Url.Builder.absolute [] []

        ToStory storyID ->
            Url.Builder.absolute [ storyID ] []



-- M O D E L


type alias Model =
    { navigationKey : Browser.Navigation.Key
    , current : Maybe String
    }


init : Maybe String -> () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init firstStoryID () url navigationKey =
    let
        initialCurrent =
            case parseUrl url of
                ToHome ->
                    firstStoryID

                ToStory storyID ->
                    Just storyID
    in
    ( { navigationKey = navigationKey
      , current = initialCurrent
      }
    , Cmd.none
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StoryMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model
            , Browser.Navigation.pushUrl model.navigationKey (Url.toString url)
            )

        UrlRequested (Browser.External path) ->
            ( model
            , Browser.Navigation.load path
            )

        UrlChanged url ->
            ( case parseUrl url of
                ToHome ->
                    { model | current = Nothing }

                ToStory storyID ->
                    { model | current = Just storyID }
            , Cmd.none
            )

        StoryMsg ->
            ( model, Cmd.none )



-- S U B S C R I P T I O N S


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- V I E W


viewLink : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewLink route attrs children =
    a (Html.Attributes.href (toPath route) :: attrs) children


viewItem : Maybe String -> Story -> Html Msg
viewItem current story =
    div
        [ style "background" (ifelse (current == Just story.title) "#ccc" "fff")
        ]
        [ viewLink (ToStory story.title)
            []
            [ text story.title
            ]
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
