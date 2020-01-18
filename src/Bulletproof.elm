module Bulletproof exposing
    ( Program
    , WithKnobs
    , initialKnobs
    , int
    , program
    , storyOf
    , string
    )

import AVL.Dict as Dict exposing (Dict)
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


type alias Model state =
    { navigationKey : Browser.Navigation.Key
    , current : Maybe String
    , state : state
    }


init : Maybe String -> state -> () -> Url -> Browser.Navigation.Key -> ( Model state, Cmd Msg )
init firstStoryID initialState () url navigationKey =
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
      , state = initialState
      }
    , Cmd.none
    )



-- U P D A T E


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | StoryMsg


update : Msg -> Model state -> ( Model state, Cmd Msg )
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


subscriptions : Model state -> Sub Msg
subscriptions _ =
    Sub.none



-- V I E W


viewLink : Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewLink route attrs children =
    a (Html.Attributes.href (toPath route) :: attrs) children


viewItem : Maybe String -> Story state view -> Html Msg
viewItem current story =
    div
        [ style "background" (ifelse (current == Just story.title) "#ccc" "fff")
        ]
        [ viewLink (ToStory story.title)
            []
            [ text story.title
            ]
        ]


viewNavigation : Maybe String -> List (Story state view) -> Html Msg
viewNavigation current =
    nav
        [ style "float" "left"
        , style "width" "30%"
        ]
        << List.map (viewItem current)


viewStory : state -> Story state (Html msg) -> Html Msg
viewStory state story =
    div
        [ style "float" "left"
        , style "width" "70%"
        ]
        [ Html.map (always StoryMsg) (story.view state)
        ]


viewEmpty : Html msg
viewEmpty =
    text "Nothing to show"


view : List (Story state (Html msg)) -> Model state -> Browser.Document Msg
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
                    |> Maybe.map (viewStory model.state)
                    |> Maybe.withDefault viewEmpty
        ]



-- A P I


type alias Story state view =
    { title : String
    , view : state -> view
    }


storyOf : String -> view -> Story state view
storyOf title view_ =
    Story title (always view_)


type alias Program state =
    Platform.Program () (Model state) Msg


program : state -> List (Story state (Html msg)) -> Program state
program initialState stories =
    Browser.application
        { init = init (Maybe.map .title <| List.head stories) initialState
        , update = update
        , view = view stories
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- K N O B S


type Knob
    = KnobInt Int
    | KnobString String


type alias KnobState =
    Dict String (Dict String Knob)


type alias WithKnobs state =
    { state | knobs : KnobState }


extractKnob : String -> String -> KnobState -> Maybe Knob
extractKnob title name state =
    state
        |> Dict.get title
        |> Maybe.andThen (Dict.get name)


initialKnobs : KnobState
initialKnobs =
    Dict.empty


int : String -> Int -> Story (WithKnobs state) (Int -> a) -> Story (WithKnobs state) a
int name defaultValue story =
    Story story.title
        (\state ->
            case extractKnob story.title name state.knobs of
                Just (KnobInt value) ->
                    story.view state value

                _ ->
                    story.view state defaultValue
        )


string : String -> String -> Story (WithKnobs state) (String -> a) -> Story (WithKnobs state) a
string name defaultValue story =
    Story story.title
        (\state ->
            case extractKnob story.title name state.knobs of
                Just (KnobString value) ->
                    story.view state value

                _ ->
                    story.view state defaultValue
        )


storyWithKnobs : Story (WithKnobs state) (Html msg)
storyWithKnobs =
    (\number str ->
        div []
            [ text (String.fromInt number)
            , text str
            ]
    )
        |> storyOf "Counter initial"
        |> int "int" 0
        |> string "str" "initial"


programWithKnobs : Program (WithKnobs {})
programWithKnobs =
    program
        { knobs = initialKnobs
        }
        [ storyWithKnobs
        ]
