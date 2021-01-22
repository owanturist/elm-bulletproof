module Router exposing (Key, parse, push, replace, toString)

import Browser.Navigation
import Story
import Url exposing (Url)
import Url.Builder


type alias Key =
    Browser.Navigation.Key


parse : Url -> Story.Path
parse url =
    case String.split "/" (String.dropLeft 1 url.path) of
        "story" :: path ->
            List.filterMap Url.percentDecode path

        _ ->
            []


toString : Story.Path -> String
toString path =
    if List.isEmpty path then
        Url.Builder.absolute [] []

    else
        Url.Builder.absolute ("story" :: path) []


replace : Browser.Navigation.Key -> Story.Path -> Cmd msg
replace key path =
    Browser.Navigation.replaceUrl key (toString path)


push : Browser.Navigation.Key -> Story.Path -> Cmd msg
push key path =
    Browser.Navigation.pushUrl key (toString path)
