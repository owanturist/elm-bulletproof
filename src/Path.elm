module Path exposing (Path(..), compare, toList, toStoryTitle)


type Path
    = Alone String
    | Component String String
    | Folder String String String


toStoryTitle : Path -> String
toStoryTitle path =
    case path of
        Alone storyID ->
            storyID

        Component _ storyID ->
            storyID

        Folder _ _ storyID ->
            storyID


toList : Path -> List String
toList path =
    case path of
        Alone storyID ->
            [ storyID ]

        Component componentID storyID ->
            [ componentID, storyID ]

        Folder folderID componentID storyID ->
            [ folderID, componentID, storyID ]


compare : Path -> Path -> Order
compare left right =
    Basics.compare (toList left) (toList right)
