module Router exposing (Route(..), parse, replace, toString)

import Browser.Navigation
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), Parser, s, string, top)


type Route
    = ToHome
    | ToStory String


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map ToHome top
        , Url.Parser.map (Maybe.withDefault ToHome << Maybe.map ToStory << Url.percentDecode) (s "story" </> string)
        ]


parse : Url -> Route
parse =
    Maybe.withDefault ToHome << Url.Parser.parse parser


toString : Route -> String
toString route =
    case route of
        ToHome ->
            Url.Builder.absolute [] []

        ToStory storyID ->
            Url.Builder.absolute [ "story", storyID ] []


replace : Browser.Navigation.Key -> Route -> Cmd msg
replace key route =
    Browser.Navigation.replaceUrl key (toString route)
