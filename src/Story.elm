module Story exposing (Path, Story(..), Workspace, get, getFirst, getNext, getPrev, isEmpty, map)

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
    | Folder String (Story view)
    | Batch (List (Story view))


isEmpty : Story view -> Bool
isEmpty story =
    case story of
        Folder _ substory ->
            isEmpty substory

        Batch stories ->
            List.all isEmpty stories

        _ ->
            False


map : (Workspace a -> Workspace b) -> Story a -> Story b
map tagger story =
    case story of
        Label title ->
            Label title

        Todo title ->
            Todo title

        Single title workspace ->
            Single title (tagger workspace)

        Folder title substory ->
            Folder title (map tagger substory)

        Batch stories ->
            Batch (List.map (map tagger) stories)


type alias Path =
    List String


type alias Connection view =
    { prev : Path
    , next : Path
    , workspace : Workspace view
    }


type alias Store view =
    { first : Maybe Path
    , last : Maybe Path
    , connections : Dict Path (Connection view)
    }


emptyStore : Store view
emptyStore =
    Store Nothing Nothing Dict.empty


makeStore : Story view -> Store view
makeStore story =
    let
        ( _, storeL ) =
            makeStoreL [] story ( Nothing, emptyStore )

        ( _, { first, last, connections } ) =
            makeStoreR [] story ( Nothing, storeL )
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


makeStoreL : Path -> Story view -> ( Maybe Path, Store view ) -> ( Maybe Path, Store view )
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

        Folder folderID substory ->
            makeStoreL (folderID :: path) substory ( prevStory, store )

        Batch stories ->
            List.foldl (makeStoreL path) ( prevStory, store ) stories

        _ ->
            ( prevStory, store )


makeStoreR : Path -> Story view -> ( Maybe Path, Store view ) -> ( Maybe Path, Store view )
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

        Folder folderID substory ->
            makeStoreR (folderID :: path) substory ( nextStory, store )

        Batch stories ->
            List.foldr (makeStoreR path) ( nextStory, store ) stories

        _ ->
            ( nextStory, store )


get : Path -> Story (Html ()) -> Maybe (Workspace (Html ()))
get path story =
    let
        store =
            makeStore story
    in
    Maybe.map .workspace (Dict.get path store.connections)


getNext : Path -> Story (Html ()) -> Maybe Path
getNext path story =
    let
        store =
            makeStore story
    in
    Maybe.andThen
        (\connection ->
            if connection.next == path then
                Nothing

            else
                Just connection.next
        )
        (Dict.get path store.connections)


getPrev : Path -> Story (Html ()) -> Maybe Path
getPrev path story =
    let
        store =
            makeStore story
    in
    Maybe.andThen
        (\connection ->
            if connection.prev == path then
                Nothing

            else
                Just connection.prev
        )
        (Dict.get path store.connections)


getFirstHelp : List (Story view) -> Maybe Path
getFirstHelp stories =
    case stories of
        [] ->
            Nothing

        head :: tail ->
            case getFirst head of
                Nothing ->
                    getFirstHelp tail

                just ->
                    just


getFirst : Story view -> Maybe Path
getFirst story =
    case story of
        Single title _ ->
            Just [ title ]

        Folder title substory ->
            Maybe.map ((::) title) (getFirst substory)

        Batch stories ->
            getFirstHelp stories

        _ ->
            Nothing
