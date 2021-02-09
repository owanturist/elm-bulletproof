module Story exposing (Path, Story(..), Workspace, get, getFirst, getNext, getPrev, map)

import Dict exposing (Dict)
import Html.Styled exposing (Html)
import Knob exposing (Knob)


type alias Workspace view =
    { knobs : List ( String, Knob )
    , view : Knob.Payload -> Maybe view
    }


type Story view
    = Label String
    | Todo String
    | Single String (Workspace view)
    | Folder String (List (Story view))


map : (Workspace a -> Workspace b) -> Story a -> Story b
map tagger story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Folder title stories ->
            Folder title (List.map (map tagger) stories)

        Single title workspace ->
            Single title (tagger workspace)


type alias Path =
    List String


type alias Connection =
    { prev : Path
    , next : Path
    , workspace : Workspace (Html ())
    }


type alias Store =
    { first : Maybe Path
    , last : Maybe Path
    , connections : Dict Path Connection
    }


emptyStore : Store
emptyStore =
    Store Nothing Nothing Dict.empty


makeStore : List (Story (Html ())) -> Store
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


makeStoreL : Path -> Story (Html ()) -> ( Maybe Path, Store ) -> ( Maybe Path, Store )
makeStoreL path story ( prevStory, store ) =
    case story of
        Single storyID workspace ->
            let
                storyPath =
                    List.reverse (storyID :: path)
            in
            ( Just storyPath
            , case prevStory of
                Nothing ->
                    { first = Just storyPath
                    , last = store.last
                    , connections = Dict.insert storyPath (Connection [] [] workspace) store.connections
                    }

                Just prevPath ->
                    { first = store.first
                    , last = store.last
                    , connections = Dict.insert storyPath (Connection prevPath [] workspace) store.connections
                    }
            )

        Folder folderID substories ->
            List.foldl (makeStoreL (folderID :: path)) ( prevStory, store ) substories

        _ ->
            ( prevStory, store )


makeStoreR : Path -> Story (Html ()) -> ( Maybe Path, Store ) -> ( Maybe Path, Store )
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

        Folder folderID substories ->
            List.foldr (makeStoreR (folderID :: path)) ( nextStory, store ) substories

        _ ->
            ( nextStory, store )


get : Path -> List (Story (Html ())) -> Maybe (Workspace (Html ()))
get path stories =
    let
        store =
            makeStore stories
    in
    Maybe.map .workspace (Dict.get path store.connections)


getNext : Path -> List (Story (Html ())) -> Maybe Path
getNext path stories =
    let
        store =
            makeStore stories
    in
    Maybe.andThen
        (\connection ->
            if connection.next == path then
                Nothing

            else
                Just connection.next
        )
        (Dict.get path store.connections)


getPrev : Path -> List (Story (Html ())) -> Maybe Path
getPrev path stories =
    let
        store =
            makeStore stories
    in
    Maybe.andThen
        (\connection ->
            if connection.prev == path then
                Nothing

            else
                Just connection.prev
        )
        (Dict.get path store.connections)


getFirst : List (Story (Html ())) -> Maybe Path
getFirst stories =
    makeStore stories
        |> .first
