module Story exposing (Path, Story(..), Workspace, getFirstPath, getNextPath, getPrevPath, getWorkspace, isEmpty, map)

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


getWorkspaceHelp : Path -> List (Story view) -> Maybe (Workspace view)
getWorkspaceHelp path stories =
    case stories of
        [] ->
            Nothing

        head :: tail ->
            case getWorkspace path head of
                Nothing ->
                    getWorkspaceHelp path tail

                just ->
                    just


getWorkspace : Path -> Story view -> Maybe (Workspace view)
getWorkspace path story =
    case ( path, story ) of
        ( fragment :: [], Single title workspace ) ->
            if fragment == title then
                Just workspace

            else
                Nothing

        ( fragment :: rest, Folder title substory ) ->
            if fragment == title then
                getWorkspace rest substory

            else
                Nothing

        ( _, Batch stories ) ->
            getWorkspaceHelp path stories

        _ ->
            Nothing


getFirstPathHelp : List (Story view) -> Maybe Path
getFirstPathHelp stories =
    case stories of
        [] ->
            Nothing

        head :: tail ->
            case getFirstPath head of
                Nothing ->
                    getFirstPathHelp tail

                just ->
                    just


getFirstPath : Story view -> Maybe Path
getFirstPath story =
    case story of
        Single title _ ->
            Just [ title ]

        Folder title substory ->
            Maybe.map ((::) title) (getFirstPath substory)

        Batch stories ->
            getFirstPathHelp stories

        _ ->
            Nothing


type GetNextResult
    = FoundCurrent Bool
    | FoundNextPath Path


getAfterStep : Path -> List (Story view) -> GetNextResult
getAfterStep path stories =
    case stories of
        [] ->
            FoundCurrent False

        head :: tail ->
            case getAfter path head of
                -- didn't find the next here - check the rest
                FoundCurrent False ->
                    getAfterStep path tail

                -- found the next here - extract the first from the rest
                FoundCurrent True ->
                    -- indicate that current has been found if rest is empty
                    case getFirstPathHelp tail of
                        Nothing ->
                            FoundCurrent True

                        Just nextPath ->
                            FoundNextPath nextPath

                foundPath ->
                    foundPath


getAfter : Path -> Story view -> GetNextResult
getAfter path story =
    case ( path, story ) of
        ( fragment :: [], Single title _ ) ->
            FoundCurrent (title == fragment)

        ( fragment :: rest, Folder title substory ) ->
            -- don't move further if path does not match
            if title == fragment then
                case getAfter rest substory of
                    FoundNextPath nextPath ->
                        FoundNextPath (title :: nextPath)

                    foundcurrent ->
                        foundcurrent

            else
                FoundCurrent False

        ( _, Batch stories ) ->
            -- actual searching for after happens here
            getAfterStep path stories

        _ ->
            FoundCurrent False


getNextPath : Path -> Story view -> Maybe Path
getNextPath path story =
    case getAfter path story of
        -- not found current - nothing can be next
        -- possible for empty stories or for not existing path
        FoundCurrent False ->
            Nothing

        -- found current but didn't find next
        -- possible when current is the last so get first as next
        -- for single story nothing can be next
        FoundCurrent True ->
            Maybe.andThen
                (\firstPath ->
                    if firstPath == path then
                        Nothing

                    else
                        Just firstPath
                )
                (getFirstPath story)

        FoundNextPath nextPath ->
            Just nextPath


getPrevPath : Path -> Story view -> Maybe Path
getPrevPath path story =
    getNextPath path (reverse story)
