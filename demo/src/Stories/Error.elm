module Stories.Error exposing (story)

import Bulletproof
import Bulletproof.Knob
import Error exposing (Error)


stringToPath : String -> List String
stringToPath =
    List.map String.trim << String.split "/"


defaultPath : List String
defaultPath =
    [ "Folder 1", "Folder 2", "Folder 3" ]


story : Bulletproof.Story
story =
    Bulletproof.folderOf "Error"
        [ Bulletproof.label "Layout"
        , Bulletproof.storyOf "Empty"
            (Error.view []
                |> Bulletproof.fromElmCss
            )
        , Bulletproof.storyOf "Single Error"
            (Error.view
                [ Error defaultPath Error.EmptyLabelTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Paths"
            (\title path n ->
                Error.view
                    [ Error [] Error.EmptyLabelTitle
                    , Error [ title ] Error.EmptyLabelTitle
                    , Error (stringToPath path) Error.EmptyLabelTitle
                    , Error [ String.repeat n "LongTitle" ] Error.EmptyLabelTitle
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Single Folder Title" "Folder"
            |> Bulletproof.Knob.string "Path" (String.join " / " defaultPath)
            |> Bulletproof.Knob.int "Long Title Repeats"
                30
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 10
                , Bulletproof.Knob.max 50
                ]

        --
        , Bulletproof.label "Stories"

        --
        , Bulletproof.storyOf "Empty Label Title"
            (Error.view
                [ Error defaultPath Error.EmptyLabelTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Empty Todo Title"
            (Error.view
                [ Error defaultPath Error.EmptyTodoTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Empty Story Title"
            (Error.view
                [ Error defaultPath Error.EmptyStoryTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Empty Folder Title"
            (Error.view
                [ Error defaultPath Error.EmptyFolderTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.storyOf "Duplicate Labels"
            (\title n ->
                Error.view
                    [ Error defaultPath (Error.DuplicateLabels title n)
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Label title" "Duplicated title"
            |> Bulletproof.Knob.int "Duplicates"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 10
                ]

        --
        , Bulletproof.storyOf "Duplicate Stories"
            (\title n ->
                Error.view
                    [ Error defaultPath (Error.DuplicateStories title n)
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Story title" "Duplicated title"
            |> Bulletproof.Knob.int "Duplicates"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 10
                ]

        --
        , Bulletproof.storyOf "Duplicate Folders"
            (\title n ->
                Error.view
                    [ Error defaultPath (Error.DuplicateFolders title n)
                    ]
                    |> Bulletproof.fromElmCss
            )
            |> Bulletproof.Knob.string "Folder title" "Duplicated title"
            |> Bulletproof.Knob.int "Duplicates"
                3
                [ Bulletproof.Knob.range
                , Bulletproof.Knob.min 2
                , Bulletproof.Knob.max 10
                ]

        --
        , Bulletproof.label "Knobs"

        --
        , Bulletproof.storyOf "Empty Knob Title"
            (Error.view
                [ Error defaultPath Error.EmptyKnobTitle
                ]
                |> Bulletproof.fromElmCss
            )

        --
        , Bulletproof.todo "Duplicate Knob"
        , Bulletproof.todo "Empty Radio"
        , Bulletproof.todo "Empty Select"
        , Bulletproof.todo "Duplicate RadioOptions"
        , Bulletproof.todo "Duplicate SelectOptions"
        , Bulletproof.todo "Invalid Int Step"
        , Bulletproof.todo "Invalid Int Left Boundary"
        , Bulletproof.todo "Invalid Int Right Boundary"
        , Bulletproof.todo "Invalid Int Boundaries"
        , Bulletproof.todo "Invalid Float Step"
        , Bulletproof.todo "Invalid Float Left Boundary"
        , Bulletproof.todo "Invalid Float Right Boundary"
        , Bulletproof.todo "Invalid Float Boundaries"
        , Bulletproof.todo "Invalid Color"
        , Bulletproof.todo "Invalid Date"
        , Bulletproof.todo "Invalid Time"
        ]
