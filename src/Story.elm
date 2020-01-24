module Story exposing (Payload, Story(..), find, firstPath)

import Addons exposing (Addons)
import Knob exposing (Knob)
import Renderer exposing (Renderer)


type Story view
    = Single String (Payload view)
    | Batch String (List (Story Renderer))


type alias Payload view =
    { knobs : List ( String, Knob )
    , view : Result String (Addons -> view)
    }


find : List String -> List (Story Renderer) -> Maybe (Payload Renderer)
find path stories =
    List.foldl
        (\story result ->
            if result /= Nothing then
                result

            else
                case ( path, story ) of
                    ( fragmentID :: [], Single storyID payload ) ->
                        if fragmentID == storyID then
                            Just payload

                        else
                            Nothing

                    ( fragmentID :: restPath, Batch folderID substories ) ->
                        if fragmentID == folderID then
                            find restPath substories

                        else
                            Nothing

                    _ ->
                        Nothing
        )
        Nothing
        stories


firstPath : List (Story Renderer) -> List String
firstPath stories =
    List.foldl
        (\story path ->
            if List.isEmpty path then
                case story of
                    Single storyID _ ->
                        [ storyID ]

                    Batch folderID substories ->
                        folderID :: firstPath substories

            else
                path
        )
        []
        stories
