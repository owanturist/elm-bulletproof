module Bulletproof exposing
    ( Program
    , program
    , storyOf
    )

import Browser
import Browser.Navigation
import Html exposing (Html, a, div, hr, nav, text)
import Html.Attributes exposing (style)
import Internal exposing (Addons, initialAddons)
import Internal.Knob as Knob
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


type alias State =
    { navigationKey : Browser.Navigation.Key
    , current : Maybe String
    }


type Model
    = Model Addons State


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
    ( Model
        initialAddons
        { navigationKey = navigationKey
        , current = initialCurrent
        }
    , Cmd.none
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
            ( case parseUrl url of
                ToHome ->
                    Model addons { state | current = Nothing }

                ToStory storyID ->
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


viewLink : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewLink route attrs children =
    a (Html.Attributes.href (toPath route) :: attrs) children


viewItem : Maybe String -> Internal.Story a -> Html Msg
viewItem currentID (Internal.Story story) =
    div
        [ style "background" (ifelse (currentID == Just story.title) "#ccc" "fff")
        ]
        [ viewLink (ToStory story.title)
            []
            [ text story.title
            ]
        ]


viewNavigation : Maybe String -> List (Story a) -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : Addons -> Internal.Story (Html msg) -> Html Msg
viewStory addons (Internal.Story story) =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ case story.view of
            Err error ->
                text error

            Ok storyView ->
                Html.map (always StoryMsg) (storyView addons)
        , hr [] []
        , Html.map KnobMsg (Knob.view story.title story.knobs addons.knobs)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


view : List (Story (Html msg)) -> Model -> Browser.Document Msg
view stories (Model addons state) =
    Browser.Document "Bulletproof"
        [ viewNavigation state.current stories
        , case state.current of
            Nothing ->
                viewEmpty

            Just current ->
                case List.filter (\(Internal.Story story) -> story.title == current) stories of
                    [] ->
                        viewEmpty

                    currentStory :: _ ->
                        viewStory addons currentStory
        ]



-- A P I


type alias Story view =
    Internal.Story view


storyOf : String -> view -> Story view
storyOf title view_ =
    Internal.Story
        { title = title
        , knobs = []
        , view = Ok (\_ -> view_)
        }


type alias Program =
    Platform.Program () Model Msg


program : List (Story (Html Msg)) -> Program
program stories =
    let
        firstStoryID =
            case stories of
                [] ->
                    Nothing

                (Internal.Story story) :: _ ->
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
