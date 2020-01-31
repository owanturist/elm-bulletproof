module Story exposing (Path, Payload, Store, Story(..), get, makeStore, next, prev)

import AVL.Dict as Dict exposing (Dict)
import Knob exposing (Knob)
import Renderer exposing (Renderer)


type Story view
    = Label String
    | Todo String
    | Single String (Payload view)
    | Batch String (List (Story Renderer))


type alias Payload view =
    { knobs : List ( String, Knob )
    , view : Result String (Knob.State -> view)
    }


type alias Path =
    List String


type alias Connection =
    { prev : Path
    , next : Path
    , payload : Payload Renderer
    }


type alias Store =
    { first : Maybe Path
    , last : Maybe Path
    , connections : Dict Path Connection
    }


emptyStore : Store
emptyStore =
    Store Nothing Nothing Dict.empty


makeStore : List (Story Renderer) -> Store
makeStore stories =
    let
        ( _, storeL ) =
            List.foldl (makeStoreL []) ( Nothing, emptyStore ) stories

        ( _, { first, last, connections } ) =
            List.foldr (makeStoreR []) ( Nothing, storeL ) stories
    in
    case Maybe.map2 Tuple.pair first last of
        Nothing ->
            emptyStore

        Just ( firstPath, lastPath ) ->
            case Maybe.map2 Tuple.pair (Dict.get firstPath connections) (Dict.get lastPath connections) of
                Nothing ->
                    emptyStore

                Just ( firstConnection, lastConnection ) ->
                    connections
                        |> Dict.insert firstPath { firstConnection | prev = lastPath }
                        |> Dict.insert lastPath { lastConnection | next = firstPath }
                        |> Store first last


makeStoreL : Path -> Story Renderer -> ( Maybe Path, Store ) -> ( Maybe Path, Store )
makeStoreL path story ( prevStory, store ) =
    case story of
        Single storyID payload ->
            let
                storyPath =
                    List.reverse (storyID :: path)
            in
            ( Just storyPath
            , case prevStory of
                Nothing ->
                    { first = Just storyPath
                    , last = store.last
                    , connections = Dict.insert storyPath (Connection [] [] payload) store.connections
                    }

                Just prevPath ->
                    { first = store.first
                    , last = store.last
                    , connections = Dict.insert storyPath (Connection prevPath [] payload) store.connections
                    }
            )

        Batch folderID substories ->
            List.foldl (makeStoreL (folderID :: path)) ( prevStory, store ) substories

        _ ->
            ( prevStory, store )


makeStoreR : Path -> Story Renderer -> ( Maybe Path, Store ) -> ( Maybe Path, Store )
makeStoreR path story ( nextStory, store ) =
    case story of
        Single storyID _ ->
            let
                storyPath =
                    List.reverse (storyID :: path)
            in
            case Dict.get storyPath store.connections of
                Nothing ->
                    ( nextStory, store )

                Just connection ->
                    ( Just storyPath
                    , case nextStory of
                        Nothing ->
                            { first = store.first
                            , last = Just storyPath
                            , connections = Dict.insert storyPath connection store.connections
                            }

                        Just nextPath ->
                            { first = store.first
                            , last = store.last
                            , connections = Dict.insert storyPath { connection | next = nextPath } store.connections
                            }
                    )

        Batch folderID substories ->
            List.foldr (makeStoreR (folderID :: path)) ( nextStory, store ) substories

        _ ->
            ( nextStory, store )


get : Path -> Store -> Maybe (Payload Renderer)
get path store =
    Maybe.map .payload (Dict.get path store.connections)


next : Path -> Store -> Maybe Path
next path store =
    Maybe.andThen
        (\connection ->
            if connection.next == path then
                Nothing

            else
                Just connection.next
        )
        (Dict.get path store.connections)


prev : Path -> Store -> Maybe Path
prev path store =
    Maybe.andThen
        (\connection ->
            if connection.prev == path then
                Nothing

            else
                Just connection.prev
        )
        (Dict.get path store.connections)
