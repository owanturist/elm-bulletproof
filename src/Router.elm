module Router exposing (Key, Route(..), parse, push, replace, toString)

import Browser.Navigation
import Url exposing (Url)
import Url.Builder


type alias Key =
    Browser.Navigation.Key


type Route
    = ToStory (List String)
    | ToNotFound


parse : Url -> Route
parse url =
    case String.split "/" (String.dropLeft 1 url.path) of
        "story" :: path ->
            ToStory (List.filterMap Url.percentDecode path)

        _ ->
            ToNotFound


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
