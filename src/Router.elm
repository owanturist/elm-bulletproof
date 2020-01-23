module Router exposing (Route(..), parse, replace, toString)

import Browser.Navigation
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), Parser, s, string)


type Route
    = ToStory (List String)
    | ToNotFound


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map
            (\storyID -> ToStory [ storyID ])
            (s "story" </> string)

        --
        , Url.Parser.map
            (\componentID storyID -> ToStory [ componentID, storyID ])
            (s "story" </> string </> string)

        --
        , Url.Parser.map
            (\folderID componentID storyID -> ToStory [ folderID, componentID, storyID ])
            (s "story" </> string </> string </> string)
        ]


parse : Url -> Route
parse =
    Maybe.withDefault ToNotFound << Url.Parser.parse parser


toString : Route -> String
toString route =
    case route of
        ToStory path ->
            Url.Builder.absolute ("story" :: path) []

        ToNotFound ->
            Url.Builder.absolute [] []


replace : Browser.Navigation.Key -> Route -> Cmd msg
replace key route =
    Browser.Navigation.replaceUrl key (toString route)
