module Error exposing (Error, Reason(..), validateStories, view)

import Color
import Css
import Date
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, code, div, p, pre, styled, text)
import Html.Styled.Attributes as Attributes
import Knob exposing (Knob)
import List
import Palette
import Story exposing (Story)
import SyntaxHighlight
import Tuple
import Utils exposing (ifelse, textCode)


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
    , Maybe.andThen (\( min, max ) -> ifelse (min <= max) Nothing (Just (onBoth name min max))) (Maybe.map2 Tuple.pair limits.min limits.max)
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
    if List.isEmpty options then
        [ EmptyChoice choice name ]

    else if List.member "" options then
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
                ( [], initialFolderCounters )
                stories


validateStoriesHelp : Story.Path -> List (Story view) -> List Error
validateStoriesHelp path stories =
    let
        ( errors, counters ) =
            List.foldr
                (\story ( allErrors, folderCounters ) ->
                    Tuple.mapFirst
                        (\newErrors -> newErrors ++ allErrors)
                        (validateStory path story folderCounters)
                )
                ( [], initialFolderCounters )
                stories
    in
    errors ++ incountFolder (List.reverse path) counters


validateStories : List (Story view) -> List Error
validateStories =
    validateStoriesHelp []


type alias Explanation msg =
    { label : List (Html msg)
    , description : List (Html msg)
    , code : String
    , diffs : List ( SyntaxHighlight.Highlight, Int, Int )
    }


explanationEmptyLabelTitle : Explanation msg
explanationEmptyLabelTitle =
    Explanation
        [ textCode "Bulletproof.label"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty or blank title."
        ]
        """
[ Bulletproof.label ""
[ Bulletproof.label "Not empty title"

--
, Bulletproof.label "   "
, Bulletproof.label "Not blank title"
]
"""
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
        ]


explanationEmptyTodoTitle : Explanation msg
explanationEmptyTodoTitle =
    Explanation
        [ textCode "Bulletproof.todo"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty or blank title."
        ]
        """
[ Bulletproof.todo ""
[ Bulletproof.todo "Not empty title"

--
, Bulletproof.todo "   "
, Bulletproof.todo "Not blank title"
]
"""
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
        ]


explanationEmptyStoryTitle : Explanation msg
explanationEmptyStoryTitle =
    Explanation
        [ textCode "Bulletproof.story"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty or blank title."
        ]
        """
[ Bulletproof.story ""
[ Bulletproof.story "Not empty title"
    (burgerIcon
        |> Bulletproof.fromHtml
    )

--
, Bulletproof.story "   "
, Bulletproof.story "Not blank title"
    (timesIcon
        |> Bulletproof.fromHtml
    )
]
"""
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 7, 8 )
        , ( SyntaxHighlight.Add, 8, 9 )
        ]


explanationEmptyFolderTitle : Explanation msg
explanationEmptyFolderTitle =
    Explanation
        [ textCode "Bulletproof.folder"
        , text " has either empty or blank title"
        ]
        [ text "Please make sure you've defined neither empty or blank title."
        ]
        """
[ Bulletproof.folder "" []
[ Bulletproof.folder "Not empty title" []

--
, Bulletproof.folder "   " []
, Bulletproof.folder "Not blank title" []
]
"""
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
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
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
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
, Bulletproof.story "Icon"
, Bulletproof.story "Icon burger"
    (burgerIcon
        |> Bulletproof.fromHtml
    )
]
"""
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
        , ( SyntaxHighlight.Del, 8, 9 )
        , ( SyntaxHighlight.Add, 9, 10 )
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
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
        , ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        ]


explanationEmptyKnobTitle : Explanation msg
explanationEmptyKnobTitle =
    Explanation
        [ textCode "Bulletproof.Knob.*"
        , text " has either empty or blank name"
        ]
        [ text "Please make sure you've defined neither empty or blank name."
        ]
        """
Bulletproof.story "Button"
    (\\buttonText buttonTabindex ->
        button
            [ tabindex buttonTabindex
            ]
            [ text buttonText
            ]
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.string "" "Funny Button"
    |> Bulletproof.Knob.string "Not empty knob" "Funny Button"
    |> Bulletproof.Knob.int "  " 0
    |> Bulletproof.Knob.int "Not blank knob" 0
"""
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
        , ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
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
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.string "Property" "Funny Button"
    |> Bulletproof.Knob.string "Text" "Funny Button"
    |> Bulletproof.Knob.int "Property" 0
    |> Bulletproof.Knob.int "Tab Index" 0
"""
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
        , ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        ]


explanationEmptyChoice : String -> String -> Explanation msg
explanationEmptyChoice choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text " has no options"
        ]
        [ text "Please make sure you've defined neither empty or blank options."
        ]
        ("""
Bulletproof.story "Button"
    (\\buttonType ->
        button
            [ type_ buttonType
            ]
            [ text "Funny Button"
            ]
            |> Bulletproof.fromHtml
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
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 15 )
        ]


explanationEmptyChoiceOption : String -> String -> Explanation msg
explanationEmptyChoiceOption choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choice ++ " \"" ++ name ++ "\" ")
        , text " has either empty or blank options"
        ]
        [ text "Please make sure you've defined neither empty or blank options."
        ]
        ("""
Bulletproof.story "Input"
    (\\inputValue ->
        input
            [ type_ "text"
            , value inputValue
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.${knob} "Input value"
        [ ( "", "" )
        [ ( "empty string", "" )
        , ( "   ", "   " )
        , ( "blank string", "   " )
        , ( "short string", "Hello World!" )
        , ( "long string", "Lorem ipsum dolor..." )
        ]
"""
            |> String.replace "${knob}" choice
        )
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        , ( SyntaxHighlight.Del, 12, 13 )
        , ( SyntaxHighlight.Add, 13, 14 )
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
            |> Bulletproof.fromHtml
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
        [ ( SyntaxHighlight.Del, 9, 12 )
        , ( SyntaxHighlight.Add, 12, 15 )
        ]


explanationInvalidIntStep : String -> Int -> Explanation msg
explanationInvalidIntStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromInt step)
        ]
        [ text "Please make sure you've defined a positive step."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        200
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step -10
        , Bulletproof.Knob.step 10
        ]
"""
        [ ( SyntaxHighlight.Del, 12, 13 )
        , ( SyntaxHighlight.Add, 13, 14 )
        ]


explanationInvalidFloatStep : String -> Float -> Explanation msg
explanationInvalidFloatStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromFloat step)
        ]
        [ text "Please make sure you've defined a positive step."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step -0.01
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ ( SyntaxHighlight.Del, 12, 13 )
        , ( SyntaxHighlight.Add, 13, 14 )
        ]


explanationInvalidIntMin : String -> Int -> Int -> Explanation msg
explanationInvalidIntMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " more than actual value"
        ]
        [ text "Please make sure the min boundary is less or equal to the value."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        200
        [ Bulletproof.Knob.min 300
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        ]


explanationInvalidFloatMin : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " more than actual value"
        ]
        [ text "Please make sure the min boundary is less or equal to the value."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0.5
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        ]


explanationInvalidIntMax : String -> Int -> Int -> Explanation msg
explanationInvalidIntMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        , text " less than actual value"
        ]
        [ text "Please make sure the max boundary is more or equal to the value."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        500
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 200
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        ]


explanationInvalidFloatMax : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        , text " less than actual value"
        ]
        [ text "Please make sure the max boundary is more or equal to the value."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 0.3
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        ]


explanationInvalidIntMinMax : String -> Int -> Int -> Explanation msg
explanationInvalidIntMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " more than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        ]
        [ text "Please make sure the min boundary less or equal to the max boundary."
        ]
        """
Bulletproof.story "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        500
        [ Bulletproof.Knob.min 2000
        [ Bulletproof.Knob.min 100
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
"""
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        ]


explanationInvalidFloatMinMax : String -> Float -> Float -> Explanation msg
explanationInvalidFloatMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " more than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        ]
        [ text "Please make sure the min boundary less or equal to the max boundary."
        ]
        """
Bulletproof.story "Progressbar"
    (\\percent ->
        progressbar
            [ style "width" (String.fromFloat (100 * percent) ++ "%")
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.float "Progress value"
        0.42
        [ Bulletproof.Knob.min 2
        [ Bulletproof.Knob.min 0
        , Bulletproof.Knob.max 1
        , Bulletproof.Knob.step 0.01
        ]
"""
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
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
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.color "Button color" "#cc0f"
    |> Bulletproof.Knob.color "Button color" "#cc0"
"""
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
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
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.date "Show date" "32-13-2020"
    |> Bulletproof.Knob.date "Show date" "02-02-2020"
"""
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
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
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.time "Show time" "24:00"
    |> Bulletproof.Knob.time "Show time" "00:00"
"""
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
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
    (\\viewport1 viewport2 ->
    (\\viewport ->
        button
            [ style "width" (String.fromInt viewport1.width ++ "px")
            , style "height" (String.fromInt viewport2.height ++ "px")
            [ style "width" (String.fromInt viewport.width ++ "px")
            , style "height" (String.fromInt viewport.height ++ "px")
            ]
            [ text "Hi, I am Elfo!"
            ]
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.viewport
    |> Bulletproof.Knob.viewport
"""
        [ ( SyntaxHighlight.Del, 1, 2 )
        , ( SyntaxHighlight.Add, 2, 3 )
        , ( SyntaxHighlight.Del, 4, 6 )
        , ( SyntaxHighlight.Add, 6, 8 )
        , ( SyntaxHighlight.Del, 14, 15 )
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

        EmptyChoiceOption choice name ->
            explanationEmptyChoiceOption choice name

        DuplicateChoiceOptions choice name option n ->
            explanationDuplicateChoiceOptions choice name option n

        InvalidIntStep name step ->
            explanationInvalidIntStep name step

        InvalidFloatStep name step ->
            explanationInvalidFloatStep name step

        InvalidIntMin name value min ->
            explanationInvalidIntMin name value min

        InvalidFloatMin name value min ->
            explanationInvalidFloatMin name value min

        InvalidIntMax name value max ->
            explanationInvalidIntMax name value max

        InvalidFloatMax name value max ->
            explanationInvalidFloatMax name value max

        InvalidIntMinMax name min max ->
            explanationInvalidIntMinMax name min max

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


styledLabel : List (Html msg) -> Html msg
styledLabel =
    styled div
        [ Css.marginBottom (Css.px 4)
        , Css.fontWeight Css.bold
        , Css.fontSize (Css.px 14)
        , Css.lineHeight (Css.px 24)
        ]
        []


styledDescription : List (Html msg) -> Html msg
styledDescription =
    styled p
        [ Css.margin3 (Css.px 12) Css.zero Css.zero
        , Css.fontSize (Css.px 13)
        ]
        []


styledPath : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledPath =
    styled code
        [ Css.padding2 (Css.px 2) (Css.px 4)
        , Css.backgroundColor Palette.smoke
        , Css.color Palette.gray
        , Css.borderRadius (Css.px 3)
        , Css.fontFamily Css.monospace
        , Css.fontSize (Css.px 10)
        ]


viewPath : List String -> Html msg
viewPath path =
    styledPath
        [ Attributes.title "Location"
        ]
        [ text ("/ " ++ String.join " / " path)
        ]


styledCodeExample : List (Html msg) -> Html msg
styledCodeExample =
    styled div
        [ Css.margin3 (Css.px 12) (Css.px -8) Css.zero
        , Css.padding2 Css.zero (Css.px 8)
        , Css.border3 (Css.px 1) Css.solid Palette.smoke
        , Css.borderRadius (Css.px 3)
        , Css.overflow Css.auto
        ]
        []


viewCodeExample : String -> List ( SyntaxHighlight.Highlight, Int, Int ) -> Html msg
viewCodeExample exampleCode diffs =
    styledCodeExample
        [ case SyntaxHighlight.elm (String.trim exampleCode) of
            Err _ ->
                pre [] [ text (String.trim exampleCode) ]

            Ok elmCode ->
                List.foldl
                    (\( highlight, start, end ) code -> SyntaxHighlight.highlightLines (Just highlight) start end code)
                    elmCode
                    diffs
                    |> SyntaxHighlight.toBlockHtml Nothing
                    |> Html.fromUnstyled
        ]


styledError : List (Html msg) -> Html msg
styledError =
    styled div
        [ Css.margin2 (Css.px 8) Css.zero
        , Css.padding3 (Css.px 12) (Css.px 16) (Css.px 8)
        , Css.backgroundColor Palette.white
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        ]
        []


viewError : Error -> Html msg
viewError error =
    let
        explanation =
            reasonToExplanation error.reason
    in
    styledError
        [ styledLabel explanation.label
        , viewPath error.path
        , styledDescription explanation.description
        , viewCodeExample explanation.code explanation.diffs
        ]


styledContainer : List (Html msg) -> Html msg
styledContainer =
    styled div
        [ Css.width (Css.px 600)
        , Css.maxWidth (Css.pct 100)
        ]
        []


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.padding2 Css.zero (Css.px 8)
        , Css.width (Css.pct 100)
        , Css.maxWidth (Css.pct 100)
        , Css.minHeight (Css.pct 100)
        , Css.backgroundColor Palette.cloud
        , Css.property "word-break" "break-word"
        , Css.color Palette.dark
        , Css.fontSize (Css.px 13)
        , Css.fontFamilies Palette.font
        ]
        []


view : List Error -> Html msg
view errors =
    styledRoot
        [ Html.fromUnstyled (SyntaxHighlight.useTheme SyntaxHighlight.gitHub)
        , styledContainer (List.map viewError errors)
        ]
