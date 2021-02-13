module Story exposing (Path, Story(..), Workspace, get, getFirst, getNext, getPrev, isEmpty, map)

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


reverse : Story view -> Story view
reverse story =
    case story of
        Batch stories ->
            stories
                |> List.map reverse
                |> List.reverse
                |> Batch

        Folder title substory ->
            Folder title (reverse substory)

        single ->
            single


type alias Path =
    List String


getHelp : Path -> List (Story view) -> Maybe (Workspace view)
getHelp path stories =
    case stories of
        [] ->
            Nothing

        head :: tail ->
            case get path head of
                Nothing ->
                    getHelp path tail

                just ->
                    just


get : Path -> Story view -> Maybe (Workspace view)
get path story =
    case ( path, story ) of
        ( fragment :: [], Single title workspace ) ->
            if fragment == title then
                Just workspace

            else
                Nothing

        ( fragment :: rest, Folder title substory ) ->
            if fragment == title then
                get rest substory

            else
                Nothing

        ( _, Batch stories ) ->
            getHelp path stories

        _ ->
            Nothing


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


type GetNextResult
    = FoundCurrent Bool
    | FoundNextPath Path


getNextStep : Path -> List (Story view) -> GetNextResult
getNextStep path stories =
    case stories of
        [] ->
            FoundCurrent False

        head :: tail ->
            case getNextHelp path head of
                FoundCurrent False ->
                    getNextStep path tail

                FoundCurrent True ->
                    case getFirstHelp tail of
                        Nothing ->
                            FoundCurrent True

                        Just nextPath ->
                            FoundNextPath nextPath

                foundPath ->
                    foundPath


getNextHelp : Path -> Story view -> GetNextResult
getNextHelp path story =
    case ( path, story ) of
        ( fragment :: [], Single title _ ) ->
            FoundCurrent (title == fragment)

        ( fragment :: rest, Folder title substory ) ->
            if title == fragment then
                case getNextHelp rest substory of
                    FoundNextPath nextPath ->
                        FoundNextPath (title :: nextPath)

                    foundcurrent ->
                        foundcurrent

            else
                FoundCurrent False

        ( _, Batch stories ) ->
            getNextStep path stories

        _ ->
            FoundCurrent False


getNext : Path -> Story view -> Maybe Path
getNext path story =
    case getNextHelp path story of
        FoundCurrent False ->
            Nothing

        FoundCurrent True ->
            Maybe.andThen
                (\firstPath ->
                    if firstPath == path then
                        Nothing

                    else
                        Just firstPath
                )
                (getFirst story)

        FoundNextPath nextPath ->
            Just nextPath


getPrev : Path -> Story view -> Maybe Path
getPrev path story =
    getNext path (reverse story)
