module Router exposing (Key, Route(..), parse, push, replace, toString)

import Browser.Navigation
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), Parser, s, string)


type alias Key =
    Browser.Navigation.Key


type Route
    = ToStory (List String)
    | ToNotFound


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map List.singleton (s "story" </> string)

        --
        , Url.Parser.map
            (\componentID storyID -> [ componentID, storyID ])
            (s "story" </> string </> string)

        --
        , Url.Parser.map
            (\folderID componentID storyID -> [ folderID, componentID, storyID ])
            (s "story" </> string </> string </> string)
        ]
        |> Url.Parser.map (ToStory << List.filterMap Url.percentDecode)


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


push : Browser.Navigation.Key -> Route -> Cmd msg
push key route =
    Browser.Navigation.pushUrl key (toString route)
