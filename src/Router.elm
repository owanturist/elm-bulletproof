module Router exposing (Route(..), parse, replace, toString)

import Browser.Navigation
import Path exposing (Path)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), Parser, s, string)


type Route
    = ToStory Path
    | ToNotFound


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Path.Alone (s "story" </> string)
        , Url.Parser.map Path.Component (s "story" </> string </> string)
        , Url.Parser.map Path.Folder (s "story" </> string </> string </> string)
        ]
        |> Url.Parser.map ToStory


parse : Url -> Route
parse =
    Maybe.withDefault ToNotFound << Url.Parser.parse parser


toString : Route -> String
toString route =
    case route of
        ToStory path ->
            Url.Builder.absolute ("story" :: Path.toList path) []

        ToNotFound ->
            Url.Builder.absolute [] []


replace : Browser.Navigation.Key -> Route -> Cmd msg
replace key route =
    Browser.Navigation.replaceUrl key (toString route)
