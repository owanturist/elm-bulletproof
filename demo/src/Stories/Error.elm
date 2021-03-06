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


layoutStory : Bulletproof.Story
layoutStory =
    [ Bulletproof.story "Empty"
        (Error.view [])

    --
    , Bulletproof.story "Single Error"
        (Error.view
            [ Error defaultPath Error.EmptyLabelTitle
            ]
        )

    --
    , Bulletproof.story "Paths"
        (\title path n ->
            Error.view
                [ Error [] Error.EmptyLabelTitle
                , Error [ title ] Error.EmptyLabelTitle
                , Error (stringToPath path) Error.EmptyLabelTitle
                , Error [ String.repeat n "LongTitle" ] Error.EmptyLabelTitle
                ]
        )
        |> Bulletproof.Knob.string "Single Folder Title" "Folder"
        |> Bulletproof.Knob.string "Path" (String.join " / " defaultPath)
        |> Bulletproof.Knob.int "Long Title Repeats"
            30
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 10
            , Bulletproof.Knob.max 50
            ]
    ]
        |> Bulletproof.batch


storiesStory : Bulletproof.Story
storiesStory =
    [ Bulletproof.story "Empty Label Title"
        (Error.view
            [ Error defaultPath Error.EmptyLabelTitle
            ]
        )

    --
    , Bulletproof.story "Empty Todo Title"
        (Error.view
            [ Error defaultPath Error.EmptyTodoTitle
            ]
        )

    --
    , Bulletproof.story "Empty Story Title"
        (Error.view
            [ Error defaultPath Error.EmptyStoryTitle
            ]
        )

    --
    , Bulletproof.story "Empty Folder Title"
        (Error.view
            [ Error defaultPath Error.EmptyFolderTitle
            ]
        )

    --
    , Bulletproof.story "Duplicate Labels"
        (\title n ->
            Error.view
                [ Error defaultPath (Error.DuplicateLabels title n)
                ]
        )
        |> Bulletproof.Knob.string "Label title" "Duplicated title"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]

    --
    , Bulletproof.story "Duplicate Stories"
        (\title n ->
            Error.view
                [ Error defaultPath (Error.DuplicateStories title n)
                ]
        )
        |> Bulletproof.Knob.string "Story title" "Duplicated title"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]

    --
    , Bulletproof.story "Duplicate Folders"
        (\title n ->
            Error.view
                [ Error defaultPath (Error.DuplicateFolders title n)
                ]
        )
        |> Bulletproof.Knob.string "Folder title" "Duplicated title"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]
    ]
        |> Bulletproof.batch


knobsStory : Bulletproof.Story
knobsStory =
    [ Bulletproof.story "Empty Knob Title"
        (Error.view
            [ Error defaultPath Error.EmptyKnobTitle
            ]
        )

    --
    , Bulletproof.story "Duplicate Knobs"
        (\name n ->
            Error.view
                [ Error defaultPath (Error.DuplicateKnobs name n)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Duplicated name"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]
    ]
        |> Bulletproof.batch


radioStory : Bulletproof.Story
radioStory =
    [ Bulletproof.story "Empty Radio"
        (\name ->
            Error.view
                [ Error defaultPath (Error.EmptyChoice "radio" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Radio name"

    --
    , Bulletproof.story "Singleton Radio"
        (\name ->
            Error.view
                [ Error defaultPath (Error.SingletonChoice "radio" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Radio name"

    --
    , Bulletproof.story "Empty Radio Option"
        (\name ->
            Error.view
                [ Error defaultPath (Error.EmptyChoiceOption "radio" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Radio name"

    --
    , Bulletproof.story "Duplicate Radio Options"
        (\name option n ->
            Error.view
                [ Error defaultPath (Error.DuplicateChoiceOptions "radio" name option n)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Radio name"
        |> Bulletproof.Knob.string "Option name" "Option name"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]
    ]
        |> Bulletproof.batch


selectStory : Bulletproof.Story
selectStory =
    [ Bulletproof.story "Empty Select"
        (\name ->
            Error.view
                [ Error defaultPath (Error.EmptyChoice "select" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Select name"

    --
    , Bulletproof.story "Singleton Select"
        (\name ->
            Error.view
                [ Error defaultPath (Error.SingletonChoice "select" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Select name"

    --
    , Bulletproof.story "Empty Select Option"
        (\name ->
            Error.view
                [ Error defaultPath (Error.EmptyChoiceOption "select" name)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Select name"

    --
    , Bulletproof.story "Duplicate Select Options"
        (\name option n ->
            Error.view
                [ Error defaultPath (Error.DuplicateChoiceOptions "select" name option n)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Select name"
        |> Bulletproof.Knob.string "Option name" "Option name"
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]
    ]
        |> Bulletproof.batch


intStory : Bulletproof.Story
intStory =
    [ Bulletproof.story "Invalid Int Step"
        (\name step ->
            Error.view
                [ Error defaultPath (Error.InvalidIntStep name step)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Int name"
        |> Bulletproof.Knob.int "Step"
            -1
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min -100
            , Bulletproof.Knob.max 0
            , Bulletproof.Knob.step 1
            ]

    --
    , Bulletproof.story "Invalid Int Min"
        (\name value min ->
            Error.view
                [ Error defaultPath (Error.InvalidIntMin name value min)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Int name"
        |> Bulletproof.Knob.int "Value"
            10
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 50
            , Bulletproof.Knob.step 1
            ]
        |> Bulletproof.Knob.int "Min"
            60
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 51
            , Bulletproof.Knob.max 100
            , Bulletproof.Knob.step 1
            ]

    --
    , Bulletproof.story "Invalid Int Max"
        (\name value max ->
            Error.view
                [ Error defaultPath (Error.InvalidIntMax name value max)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Int name"
        |> Bulletproof.Knob.int "Value"
            70
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 51
            , Bulletproof.Knob.max 100
            , Bulletproof.Knob.step 1
            ]
        |> Bulletproof.Knob.int "Max"
            20
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 50
            , Bulletproof.Knob.step 1
            ]

    --
    , Bulletproof.story "Invalid Int Min and Max"
        (\name min max ->
            Error.view
                [ Error defaultPath (Error.InvalidIntMinMax name min max)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Int name"
        |> Bulletproof.Knob.int "Min"
            70
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 51
            , Bulletproof.Knob.max 100
            , Bulletproof.Knob.step 1
            ]
        |> Bulletproof.Knob.int "Max"
            20
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 50
            , Bulletproof.Knob.step 1
            ]
    ]
        |> Bulletproof.batch


floatStory : Bulletproof.Story
floatStory =
    [ Bulletproof.story "Invalid Float Step"
        (\name step ->
            Error.view
                [ Error defaultPath (Error.InvalidFloatStep name step)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Float name"
        |> Bulletproof.Knob.float "Step"
            -0.1
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min -1
            , Bulletproof.Knob.max 0
            , Bulletproof.Knob.step 0.1
            ]

    --
    , Bulletproof.story "Invalid Float Min"
        (\name value min ->
            Error.view
                [ Error defaultPath (Error.InvalidFloatMin name value min)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Float name"
        |> Bulletproof.Knob.float "Value"
            0.2
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 0.5
            , Bulletproof.Knob.step 0.1
            ]
        |> Bulletproof.Knob.float "Min"
            0.6
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0.51
            , Bulletproof.Knob.max 1
            , Bulletproof.Knob.step 0.1
            ]

    --
    , Bulletproof.story "Invalid Float Max"
        (\name value max ->
            Error.view
                [ Error defaultPath (Error.InvalidFloatMax name value max)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Float name"
        |> Bulletproof.Knob.float "Value"
            0.6
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0.51
            , Bulletproof.Knob.max 1
            , Bulletproof.Knob.step 0.1
            ]
        |> Bulletproof.Knob.float "Max"
            0.2
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 0.5
            , Bulletproof.Knob.step 0.1
            ]

    --
    , Bulletproof.story "Invalid Float Min and Max"
        (\name min max ->
            Error.view
                [ Error defaultPath (Error.InvalidFloatMinMax name min max)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Float name"
        |> Bulletproof.Knob.float "Min"
            0.6
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0.51
            , Bulletproof.Knob.max 1
            , Bulletproof.Knob.step 0.1
            ]
        |> Bulletproof.Knob.float "Max"
            0.2
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 0
            , Bulletproof.Knob.max 0.5
            , Bulletproof.Knob.step 0.1
            ]
    ]
        |> Bulletproof.batch


colorStory : Bulletproof.Story
colorStory =
    Bulletproof.story "Invalid Color"
        (\name color ->
            Error.view
                [ Error defaultPath (Error.InvalidColor name color)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Color name"
        |> Bulletproof.Knob.string "Color" "#asd"


dateStory : Bulletproof.Story
dateStory =
    Bulletproof.story "Invalid Date"
        (\name date ->
            Error.view
                [ Error defaultPath (Error.InvalidDate name date)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Date name"
        |> Bulletproof.Knob.string "Date" "32-13-2020"


timeStory : Bulletproof.Story
timeStory =
    Bulletproof.story "Invalid Time"
        (\name time ->
            Error.view
                [ Error defaultPath (Error.InvalidTime name time)
                ]
        )
        |> Bulletproof.Knob.string "Knob name" "Time name"
        |> Bulletproof.Knob.string "Time" "60:25"


viewportStory : Bulletproof.Story
viewportStory =
    Bulletproof.story "Duplicate Viewport"
        (\n ->
            Error.view
                [ Error defaultPath (Error.DuplicateStoryViewport n)
                ]
        )
        |> Bulletproof.Knob.int "Duplicates"
            3
            [ Bulletproof.Knob.range
            , Bulletproof.Knob.min 2
            , Bulletproof.Knob.max 10
            ]


story : Bulletproof.Story
story =
    Bulletproof.folder "Error"
        [ Bulletproof.label "LAYOUT"
        , layoutStory

        --
        , Bulletproof.label "STORIES"
        , storiesStory

        --
        , Bulletproof.label "KNOBS"
        , knobsStory

        --
        , Bulletproof.label "RADIO"
        , radioStory

        --
        , Bulletproof.label "SELECT"
        , selectStory

        --
        , Bulletproof.label "INT"
        , intStory

        --
        , Bulletproof.label "FLOAT"
        , floatStory

        --
        , Bulletproof.label "COLOR"
        , colorStory

        --
        , Bulletproof.label "DATE"
        , dateStory

        --
        , Bulletproof.label "TIME"
        , timeStory

        --
        , Bulletproof.label "VIEWPORT"
        , viewportStory
        ]
