module Error exposing (Error, Reason(..))


type Reason
    = EmptyTitle
    | DuplicateLabels String
    | DuplicateStories String
    | DuplicateFolders String
    | EmptyKnob
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
