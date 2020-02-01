module Error exposing (Error, Reason(..))


type Reason
    = EmptyLabelTitle
    | EmptyStoryTitle
    | EmptyTodoTitle
    | EmptyFolderTitle
    | EmptyKnobTitle
    | DuplicateLabels String
    | DuplicateStories String
    | DuplicateFolders String
    | DuplicateKnob String
    | EmptyRadio String
    | DuplicateRadio String (List String)
    | EmptySelect String
    | DuplicateSelect String (List String)
    | InvalidColor String String
    | InvalidDate String String
    | InvalidTime String String


type alias Error =
    { path : List String
    , reason : Reason
    }
