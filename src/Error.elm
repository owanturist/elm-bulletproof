module Error exposing (Error, Reason(..), css, validateStories, view)

import Color
import Date
import Dict exposing (Dict)
import Html exposing (Html, code, div, p, pre, text)
import Html.Attributes as Attributes
import Knob exposing (Knob)
import List
import Palette
import Story exposing (Story)
import Style
import SyntaxHighlight
import TextCode exposing (textCode)
import Tuple
import Utils exposing (ifelse)


type Reason
    = EmptyLabelTitle
    | EmptyStoryTitle
    | EmptyTodoTitle
    | EmptyFolderTitle
    | EmptyKnobTitle
    | DuplicateLabels String Int
    | DuplicateStories String Int
    | DuplicateFolders String Int
    | DuplicateKnobs String Int
    | EmptyChoice String String
    | SingletonChoice String String
    | EmptyChoiceOption String String
    | DuplicateChoiceOptions String String String Int
    | InvalidIntStep String Int
    | InvalidIntMin String Int Int
    | InvalidIntMax String Int Int
    | InvalidIntMinMax String Int Int
    | InvalidFloatStep String Float
    | InvalidFloatMin String Float Float
    | InvalidFloatMax String Float Float
    | InvalidFloatMinMax String Float Float
    | InvalidColor String String
    | InvalidDate String String
    | InvalidTime String String
    | DuplicateStoryViewport Int


type alias Error =
    { path : Story.Path
    , reason : Reason
    }



-- V A L I D A T E   K N O B


validateLimits :
    { onStep : String -> number -> Reason
    , onLeft : String -> number -> number -> Reason
    , onRight : String -> number -> number -> Reason
    , onBoth : String -> number -> number -> Reason
    }
    -> String
    -> number
    -> Knob.Limits number
    -> List Reason
validateLimits { onStep, onLeft, onRight, onBoth } name number limits =
    [ Maybe.andThen (\step -> ifelse (step > 0) Nothing (Just (onStep name step))) limits.step
    , Maybe.andThen (\min -> ifelse (number >= min) Nothing (Just (onLeft name number min))) limits.min
    , Maybe.andThen (\max -> ifelse (number <= max) Nothing (Just (onRight name number max))) limits.max
    , Maybe.andThen (\( min, max ) -> ifelse (min < max) Nothing (Just (onBoth name min max))) (Maybe.map2 Tuple.pair limits.min limits.max)
    ]
        |> List.filterMap identity


validateIntLimits : String -> Int -> Knob.Limits Int -> List Reason
validateIntLimits =
    validateLimits
        { onStep = InvalidIntStep
        , onLeft = InvalidIntMin
        , onRight = InvalidIntMax
        , onBoth = InvalidIntMinMax
        }


validateFloatLimits : String -> Float -> Knob.Limits Float -> List Reason
validateFloatLimits =
    validateLimits
        { onStep = InvalidFloatStep
        , onLeft = InvalidFloatMin
        , onRight = InvalidFloatMax
        , onBoth = InvalidFloatMinMax
        }


validateChoice : String -> String -> List String -> List Reason
validateChoice choice name options =
    case options of
        [] ->
            [ EmptyChoice choice name ]

        _ :: [] ->
            [ SingletonChoice choice name ]

        _ ->
            if List.member "" options then
                [ EmptyChoiceOption choice name ]

            else
                List.map
                    (\( option, n ) -> DuplicateChoiceOptions choice name option n)
                    (countList options)


validateColor : String -> String -> List Reason
validateColor name hex =
    if Color.fromString hex == Nothing then
        [ InvalidColor name hex ]

    else
        []


validateDate : String -> String -> List Reason
validateDate name date =
    if Date.dateFromString date == Nothing then
        [ InvalidDate name date ]

    else
        []


validateTime : String -> String -> List Reason
validateTime name time =
    if Date.timeFromString time == Nothing then
        [ InvalidTime name time ]

    else
        []


validateKnob : String -> Knob -> List Reason
validateKnob name knob =
    case knob of
        Knob.Int _ int limits ->
            validateIntLimits name int limits

        Knob.Float _ float limits ->
            validateFloatLimits name float limits

        Knob.Radio options ->
            validateChoice "radio" name options

        Knob.Select options ->
            validateChoice "select" name options

        Knob.Color hex ->
            validateColor name hex

        Knob.Date date ->
            validateDate name date

        Knob.Time time ->
            validateTime name time

        _ ->
            []


validateKnobs : List ( String, Knob ) -> List Reason
validateKnobs knobs =
    let
        -- split viewport and rest of knobs
        -- to calculate duplicates independently
        ( storyViewportKnobsN, restKnobs ) =
            knobs
                |> List.partition ((==) Knob.StoryViewport << Tuple.second)
                |> Tuple.mapFirst List.length

        duplicatedKnobsReasons =
            List.map Tuple.first restKnobs
                |> countList
                |> List.map (\( name, n ) -> DuplicateKnobs name n)

        invalidKnobsReasons =
            List.concatMap
                (\( name, knob ) ->
                    if String.isEmpty name then
                        [ EmptyKnobTitle ]

                    else
                        validateKnob name knob
                )
                knobs
    in
    if storyViewportKnobsN > 1 then
        DuplicateStoryViewport storyViewportKnobsN :: duplicatedKnobsReasons ++ invalidKnobsReasons

    else
        duplicatedKnobsReasons ++ invalidKnobsReasons



-- V A L I D A T E   S T O R Y


type alias Counter =
    Dict String Int


count : String -> Counter -> Counter
count key counter =
    let
        n =
            Maybe.withDefault 0 (Dict.get key counter)
    in
    Dict.insert key (n + 1) counter


incount : Counter -> List ( String, Int )
incount counter =
    counter
        |> Dict.toList
        |> List.filter ((<) 1 << Tuple.second)


countList : List String -> List ( String, Int )
countList list =
    incount (List.foldr count Dict.empty list)


type alias FolderCounters =
    { labels : Counter
    , stories : Counter
    , folders : Counter
    }


incountFolder : Story.Path -> FolderCounters -> List Error
incountFolder path { labels, stories, folders } =
    List.concatMap
        (\( reason, incounts ) -> List.map (\( title, n ) -> Error path (reason title n)) incounts)
        [ ( DuplicateLabels, incount labels )
        , ( DuplicateStories, incount stories )
        , ( DuplicateFolders, incount folders )
        ]


initialFolderCounters : FolderCounters
initialFolderCounters =
    FolderCounters Dict.empty Dict.empty Dict.empty


validateStory : Story.Path -> Story view -> FolderCounters -> ( List Error, FolderCounters )
validateStory path story counters =
    case story of
        Story.Label "" ->
            ( [ Error (List.reverse path) EmptyLabelTitle ]
            , counters
            )

        Story.Label title ->
            ( []
            , { counters | labels = count title counters.labels }
            )

        Story.Todo "" ->
            ( [ Error (List.reverse path) EmptyTodoTitle ]
            , counters
            )

        Story.Todo title ->
            ( []
            , { counters | stories = count title counters.stories }
            )

        Story.Single "" _ ->
            ( [ Error (List.reverse path) EmptyStoryTitle ]
            , counters
            )

        Story.Single title workspace ->
            ( List.map
                (Error (List.reverse (title :: path)))
                (validateKnobs workspace.knobs)
            , { counters | stories = count title counters.stories }
            )

        Story.Folder "" _ ->
            ( [ Error (List.reverse path) EmptyFolderTitle ]
            , counters
            )

        Story.Folder title substories ->
            ( validateStoriesHelp (title :: path) substories
            , { counters | folders = count title counters.folders }
            )

        Story.Batch stories ->
            List.foldr
                (\substory ( allErrors, folderCounters ) ->
                    Tuple.mapFirst
                        (\newErrors -> newErrors ++ allErrors)
                        (validateStory path substory folderCounters)
                )
                ( [], counters )
                stories


validateStoriesHelp : Story.Path -> Story view -> List Error
validateStoriesHelp path story =
    let
        ( errors, counters ) =
            validateStory path story initialFolderCounters
    in
    errors ++ incountFolder (List.reverse path) counters


validateStories : Story view -> List Error
validateStories =
    validateStoriesHelp []


type Diff
    = Add Int Int
    | Del Int Int


applyDiff : Diff -> SyntaxHighlight.HCode -> SyntaxHighlight.HCode
applyDiff diff hcode =
    case diff of
        Add start end ->
            SyntaxHighlight.highlightLines (Just SyntaxHighlight.Add) start end hcode

        Del start end ->
            SyntaxHighlight.highlightLines (Just SyntaxHighlight.Del) start end hcode


type alias Explanation msg =
    { label : List (Html msg)
    , description : List (Html msg)
    , code : String
    , diffs : List Diff
    }


explanationEmptyLabelTitle : Explanation msg
explanationEmptyLabelTitle =
    Explanation
        [ textCode "Bulletproof.label"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty nor blank title."
        ]
        """
[ Bulletproof.label ""
[ Bulletproof.label "Not empty title"

--
, Bulletproof.label "   "
, Bulletproof.label "Not blank title"
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        ]


explanationEmptyTodoTitle : Explanation msg
explanationEmptyTodoTitle =
    Explanation
        [ textCode "Bulletproof.todo"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty nor blank title."
        ]
        """
[ Bulletproof.todo ""
[ Bulletproof.todo "Not empty title"

--
, Bulletproof.todo "   "
, Bulletproof.todo "Not blank title"
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        ]


explanationEmptyStoryTitle : Explanation msg
explanationEmptyStoryTitle =
    Explanation
        [ textCode "Bulletproof.story"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty nor blank title."
        ]
        """
[ Bulletproof.story "" burgerIcon
[ Bulletproof.story "Not empty title" burgerIcon

--
, Bulletproof.story "   " burgerIcon
, Bulletproof.story "Not blank title" burgerIcon
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        ]


explanationEmptyFolderTitle : Explanation msg
explanationEmptyFolderTitle =
    Explanation
        [ textCode "Bulletproof.folder"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty nor blank title."
        ]
        """
[ Bulletproof.folder "" []
[ Bulletproof.folder "Not empty title" []

--
, Bulletproof.folder "   " []
, Bulletproof.folder "Not blank title" []
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        ]


explanationDuplicateLabels : String -> Int -> Explanation msg
explanationDuplicateLabels title n =
    Explanation
        [ textCode ("Bulletproof.label \"" ++ title ++ "\"")
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        [ text "Please make sure you've defined unique titles for lables inside a folder." ]
        """
[ Bulletproof.label "Components"
[ Bulletproof.label "Core components"

--
, Bulletproof.label "Components"
, Bulletproof.label "Project Components"
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        ]


explanationDuplicateStories : String -> Int -> Explanation msg
explanationDuplicateStories title n =
    Explanation
        [ textCode ("Bulletproof.story \"" ++ title ++ "\"")
        , text " or "
        , textCode ("Bulletproof.todo \"" ++ title ++ "\"")
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        [ text "Please make sure you've defined unique titles for both stories and todos inside a folder."
        ]
        """
[ Bulletproof.todo "Icon"
[ Bulletproof.todo "Icon times"

--
, Bulletproof.todo "Icon"
, Bulletproof.todo "Icon user"

--
, Bulletproof.story "Icon" burgerIcon
, Bulletproof.story "Icon burger" burgerIcon
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        , Del 8 9
        , Add 9 10
        ]


explanationDuplicateFolders : String -> Int -> Explanation msg
explanationDuplicateFolders title n =
    Explanation
        [ textCode ("Bulletproof.folder \"" ++ title ++ "\"")
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        [ text "Please make sure you've defined unique titles for folders inside a folder."
        ]
        """
[ Bulletproof.folder "Searchbar" []
[ Bulletproof.folder "Searchbar home page" []

--
, Bulletproof.folder "Searchbar"
, Bulletproof.folder "Searchbar product page"
    [ Bulletproof.todo "Disabled"
    , Bulletproof.todo "Hover"
    ]

--
, Bulletproof.folder "Searchbar"
, Bulletproof.folder "Searchbar history page"
    [ Bulletproof.todo "Disabled"
    , Bulletproof.todo "Hover"
    ]
]
"""
        [ Del 0 1
        , Add 1 2
        , Del 4 5
        , Add 5 6
        , Del 11 12
        , Add 12 13
        ]


explanationEmptyKnobTitle : Explanation msg
explanationEmptyKnobTitle =
    Explanation
        [ textCode "Bulletproof.Knob.*"
        , text " has either empty or blank name"
        ]
        [ text "Please make sure you've defined neither empty nor blank name."
        ]
        """
Bulletproof.story "Button"
    (\\buttonText buttonTabindex ->
        button
            [ tabindex buttonTabindex
            ]
            [ text buttonText
            ]
    )
    |> Bulletproof.Knob.string "" "Funny Button"
    |> Bulletproof.Knob.string "Not empty knob" "Funny Button"
    |> Bulletproof.Knob.int "  " 0
    |> Bulletproof.Knob.int "Not blank knob" 0
"""
        [ Del 8 9
        , Add 9 10
        , Del 10 11
        , Add 11 12
        ]


explanationDuplicateKnobs : String -> Int -> Explanation msg
explanationDuplicateKnobs name n =
    Explanation
        [ textCode ("Bulletproof.Knob.* \"" ++ name ++ "\"")
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        [ text "Please make sure you've defined unique names for knobs inside a story."
        ]
        """
Bulletproof.story "Button"
    (\\buttonText buttonTabindex ->
        button
            [ tabindex buttonTabindex
            ]
            [ text buttonText
            ]
    )
    |> Bulletproof.Knob.string "Property" "Funny Button"
    |> Bulletproof.Knob.string "Text" "Funny Button"
    |> Bulletproof.Knob.int "Property" 0
    |> Bulletproof.Knob.int "Tab Index" 0
"""
        [ Del 8 9
        , Add 9 10
        , Del 10 11
        , Add 11 12
        ]


explanationEmptyChoice : String -> String -> Explanation msg
explanationEmptyChoice choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text " has no options"
        ]
        [ text "Please make sure you've defined any options."
        ]
        ("""
Bulletproof.story "Button"
    (\\buttonType ->
        button
            [ type_ buttonType
            ]
            [ text "Funny Button"
            ]
    )
    |> Bulletproof.Knob.${knob} "Button type"
        []
        [ ( "button", "button" )
        , ( "reset", "reset" )
        , ( "submit", "submit" )
        ]
"""
            |> String.replace "${knob}" choice
        )
        [ Del 9 10
        , Add 10 14
        ]


explanationSingletonChoice : String -> String -> Explanation msg
explanationSingletonChoice choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text " has only one option"
        ]
        [ text "Please make sure you've defined two or more options."
        ]
        ("""
Bulletproof.story "Button"
    (\\buttonType ->
        button
            [ type_ buttonType
            ]
            [ text "Funny Button"
            ]
    )
    |> Bulletproof.Knob.${knob} "Button type"
        [ ( "button", "button" )
        , ( "reset", "reset" )
        , ( "submit", "submit" )
        ]
"""
            |> String.replace "${knob}" choice
        )
        [ Add 10 12
        ]


explanationEmptyChoiceOption : String -> String -> Explanation msg
explanationEmptyChoiceOption choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text " has either empty or blank options"
        ]
        [ text "Please make sure you've defined neither empty nor blank options."
        ]
        ("""
Bulletproof.story "Input"
    (\\inputValue ->
        input
            [ type_ "text"
            , value inputValue
            ]
            []
    )
    |> Bulletproof.Knob.${knob} "Input value"
        [ ( "", "" )
        , ( "   ", "   " )
        [ ( "empty string", "" )
        , ( "blank string", "   " )
        , ( "short string", "Hello World!" )
        , ( "long string", "Lorem ipsum dolor..." )
        ]
"""
            |> String.replace "${knob}" choice
        )
        [ Del 9 11
        , Add 11 15
        ]


explanationDuplicateChoiceOptions : String -> String -> String -> Int -> Explanation msg
explanationDuplicateChoiceOptions choice name option n =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text (" has " ++ String.fromInt n ++ "\u{00A0}times repeated option ")
        , textCode ("\"" ++ option ++ "\"")
        ]
        [ text "Please make sure you've defined unique names of options."
        ]
        ("""
Bulletproof.story "Input"
    (\\inputType ->
        input
            [ type_ inputType
            ]
            []
    )
    |> Bulletproof.Knob.${knob} "Input type"
        [ ( "string", "text" )
        , ( "string", "email" )
        , ( "string", "password" )
        [ ( "plain text", "text" )
        , ( "user email", "email" )
        , ( "user password", "password" )
        ]
"""
            |> String.replace "${knob}" choice
        )
        [ Del 8 11
        , Add 11 14
        ]


explanationInvalidIntStep : String -> Int -> Explanation msg
explanationInvalidIntStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromInt step)
        ]
        [ text "Please make sure you've defined step greater than 0."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
    )
    |> Bulletproof.Knob.int "Input size"
        200
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step -10
        , Bulletproof.Knob.step 10
        ]
"""
        [ Del 11 12
        , Add 12 13
        ]


explanationInvalidIntMin : String -> Int -> Int -> Explanation msg
explanationInvalidIntMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " greater than actual value"
        ]
        [ text "Please make sure that minimum boundary is lower or equal to the value."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
    )
    |> Bulletproof.Knob.int "Input size"
        200
        [ Bulletproof.Knob.min 300
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ Del 9 10
        , Add 10 11
        ]


explanationInvalidIntMax : String -> Int -> Int -> Explanation msg
explanationInvalidIntMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        , text " lower than actual value"
        ]
        [ text "Please make sure that maximum boundary is greater or equal to the value."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
    )
    |> Bulletproof.Knob.int "Input size"
        500
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 200
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ Del 10 11
        , Add 11 12
        ]


explanationInvalidIntMinMax : String -> Int -> Int -> Explanation msg
explanationInvalidIntMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " greater than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        ]
        [ text "Please make sure that minimum boundary lower than the maximum boundary."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
    )
    |> Bulletproof.Knob.int "Input size"
        500
        [ Bulletproof.Knob.min 2000
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ Del 9 10
        , Add 10 11
        ]


explanationInvalidFloatStep : String -> Float -> Explanation msg
explanationInvalidFloatStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromFloat step)
        ]
        [ text "Please make sure you've defined step greater than 0."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step -0.01
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ Del 11 12
        , Add 12 13
        ]


explanationInvalidFloatMin : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " greater than actual value"
        ]
        [ text "Please make sure that minimum boundary is lower or equal to the value."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0.5
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ Del 9 10
        , Add 10 11
        ]


explanationInvalidFloatMax : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        , text " lower than actual value"
        ]
        [ text "Please make sure that maximum boundary is greater or equal to the value."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 0.3
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ Del 10 11
        , Add 11 12
        ]


explanationInvalidFloatMinMax : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " greater than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        ]
        [ text "Please make sure that minimum boundary lower than the maximum boundary."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 2
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ Del 9 10
        , Add 10 11
        ]


explanationInvalidColor : String -> String -> Explanation msg
explanationInvalidColor name color =
    Explanation
        [ textCode ("Bulletproof.Knob.color" ++ " \"" ++ name ++ "\" \"" ++ color ++ "\"")
        , text " has invalid color"
        ]
        [ text "Please make sure provided value is following one of the "
        , textCode "#rgb"
        , text " or "
        , textCode "#rrggbb"
        , text "hex color patterns."
        ]
        """
Bulletproof.story "Colored Button"
    (\\color ->
        button
            [ property "background-color" color.hex
            ]
            [ text "Funny Button"
            ]
    )
    |> Bulletproof.Knob.color "Button color" "#cc0f"
    |> Bulletproof.Knob.color "Button color" "#cc0"
"""
        [ Del 8 9
        , Add 9 10
        ]


explanationInvalidDate : String -> String -> Explanation msg
explanationInvalidDate name date =
    Explanation
        [ textCode ("Bulletproof.Knob.date" ++ " \"" ++ name ++ "\" \"" ++ date ++ "\"")
        , text " has invalid date"
        ]
        [ text "Please make sure provided value is following one of the "
        , textCode "dd-mm-yyyy"
        , text ", "
        , textCode "dd/mm/yyyy"
        , text " or "
        , textCode "dd.mm.yyyy"
        , text " date patterns."
        ]
        """
Bulletproof.story "Date show"
    (\\date ->
        div
            []
            [ text ("Day " ++ String.fromInt date.day)
            , text (", Month " ++ String.fromInt date.month)
            , text (", Year " ++ String.fromInt date.year)
            ]
    )
    |> Bulletproof.Knob.date "Show date" "32-13-2020"
    |> Bulletproof.Knob.date "Show date" "29-02-2020"
"""
        [ Del 9 10
        , Add 10 11
        ]


explanationInvalidTime : String -> String -> Explanation msg
explanationInvalidTime name time =
    Explanation
        [ textCode ("Bulletproof.Knob.time" ++ " \"" ++ name ++ "\" \"" ++ time ++ "\"")
        , text " has invalid time "
        ]
        [ text "Please make sure provided value is following the "
        , textCode "mm:hh"
        , text " time pattern."
        ]
        """
Bulletproof.story "Time show"
    (\\time ->
        div
            []
            [ text ("Minutes " ++ String.fromInt time.minutes)
            , text (", Hours " ++ String.fromInt time.hours)
            ]
    )
    |> Bulletproof.Knob.time "Show time" "24:00"
    |> Bulletproof.Knob.time "Show time" "00:00"
"""
        [ Del 8 9
        , Add 9 10
        ]


explanationDuplicateStoryViewport : Int -> Explanation msg
explanationDuplicateStoryViewport n =
    Explanation
        [ textCode "Bulletproof.Knob.viewport"
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        [ text "Please make sure you've used the viewport knob only once inside a story."
        ]
        """
Bulletproof.story "Button"
    (\\{ width } { height } ->
    (\\{ width, height } ->
        button
            [ style "width" (String.fromInt width ++ "px")
            , style "height" (String.fromInt height ++ "px")
            [ text "Hi, I'm Elfo!"
            ]
    )
    |> Bulletproof.Knob.viewport
    |> Bulletproof.Knob.viewport
"""
        [ Del 1 2
        , Add 2 3
        , Del 10 11
        ]


reasonToExplanation : Reason -> Explanation msg
reasonToExplanation reason =
    case reason of
        EmptyLabelTitle ->
            explanationEmptyLabelTitle

        EmptyTodoTitle ->
            explanationEmptyTodoTitle

        EmptyStoryTitle ->
            explanationEmptyStoryTitle

        EmptyFolderTitle ->
            explanationEmptyFolderTitle

        DuplicateLabels title n ->
            explanationDuplicateLabels title n

        DuplicateStories title n ->
            explanationDuplicateStories title n

        DuplicateFolders title n ->
            explanationDuplicateFolders title n

        EmptyKnobTitle ->
            explanationEmptyKnobTitle

        DuplicateKnobs name n ->
            explanationDuplicateKnobs name n

        EmptyChoice choice name ->
            explanationEmptyChoice choice name

        SingletonChoice choice name ->
            explanationSingletonChoice choice name

        EmptyChoiceOption choice name ->
            explanationEmptyChoiceOption choice name

        DuplicateChoiceOptions choice name option n ->
            explanationDuplicateChoiceOptions choice name option n

        InvalidIntStep name step ->
            explanationInvalidIntStep name step

        InvalidIntMin name value min ->
            explanationInvalidIntMin name value min

        InvalidIntMax name value max ->
            explanationInvalidIntMax name value max

        InvalidIntMinMax name min max ->
            explanationInvalidIntMinMax name min max

        InvalidFloatStep name step ->
            explanationInvalidFloatStep name step

        InvalidFloatMin name value min ->
            explanationInvalidFloatMin name value min

        InvalidFloatMax name value max ->
            explanationInvalidFloatMax name value max

        InvalidFloatMinMax name min max ->
            explanationInvalidFloatMinMax name min max

        InvalidColor name color ->
            explanationInvalidColor name color

        InvalidDate name date ->
            explanationInvalidDate name date

        InvalidTime name time ->
            explanationInvalidTime name time

        DuplicateStoryViewport n ->
            explanationDuplicateStoryViewport n


css : Style.Sheet
css =
    Style.elements
        [ error__root
        , error__container
        , error__error
        , error__path
        , error__label
        , error__description
        , error__code_example
        ]


error__root : Style.Element
error__root =
    Style.el "error__root"
        [ Style.rule "box-sizing" "border-box"
        , Style.rule "display" "flex"
        , Style.rule "flex-direction" "column"
        , Style.rule "align-items" "center"
        , Style.rule "padding" "0 8px"
        , Style.rule "width" "100%"
        , Style.rule "max-width" "100%"
        , Style.rule "min-height" "100%"
        , Style.rule "background" Palette.cloud
        , Style.rule "color" Palette.dark
        , Style.rule "font-size" "13px"
        , Style.rule "font-family" Palette.font
        , Style.rule "word-break" "break-word"
        ]


error__container : Style.Element
error__container =
    Style.el "error__container"
        [ Style.rule "width" "600px"
        , Style.rule "max-width" "100%"
        ]


error__error : Style.Element
error__error =
    Style.el "error__error"
        [ Style.rule "margin" "8px 0"
        , Style.rule "padding" "12px 16px 8px"
        , Style.rule "background" Palette.white
        , Style.rule "box-shadow" ("0 0 10px " ++ Palette.smoke)
        ]


error__path : Style.Element
error__path =
    Style.el "error__path"
        [ Style.rule "padding" "2px 4px"
        , Style.rule "background" Palette.smoke
        , Style.rule "color" Palette.gray
        , Style.rule "border-radius" "3px"
        , Style.rule "font-size" "10px"
        , Style.rule "font-family" "monospace"
        , Style.rule "line-height" "2"
        ]


error__label : Style.Element
error__label =
    Style.el "error__label"
        [ Style.rule "margin-top" "6px"
        , Style.rule "font-weight" "bold"
        , Style.rule "font-size" "14px"
        , Style.rule "line-height" "24px"
        ]


error__description : Style.Element
error__description =
    Style.el "error__description"
        [ Style.rule "margin" "8px 0 0"
        , Style.rule "font-size" "13px"
        ]


error__code_example : Style.Element
error__code_example =
    Style.el "error__code_example"
        [ Style.rule "margin" "12px -8px 0"
        , Style.rule "padding" "0 8px"
        , Style.rule "border" ("1px solid " ++ Palette.smoke)
        , Style.rule "border-radius" "3px"
        , Style.rule "overflow" "auto"
        ]


viewPath : List String -> Html msg
viewPath path =
    code
        [ Style.class error__path
        , Attributes.title "Location"
        ]
        [ text ("/ " ++ String.join " / " path)
        ]


viewCodeExample : String -> List Diff -> Html msg
viewCodeExample exampleCode diffs =
    div
        [ Style.class error__code_example
        ]
        [ case SyntaxHighlight.elm (String.trim exampleCode) of
            Err _ ->
                pre [] [ text (String.trim exampleCode) ]

            Ok elmCode ->
                List.foldl applyDiff elmCode diffs
                    |> SyntaxHighlight.toBlockHtml Nothing
        ]


viewError : Error -> Html msg
viewError error =
    let
        explanation =
            reasonToExplanation error.reason
    in
    div
        [ Style.class error__error
        ]
        [ viewPath error.path
        , div [ Style.class error__label ] explanation.label
        , p [ Style.class error__description ] explanation.description
        , viewCodeExample explanation.code explanation.diffs
        ]


view : List Error -> Html msg
view errors =
    div
        [ Style.class error__root
        ]
        [ div [ Style.class error__container ] (List.map viewError errors)
        ]
