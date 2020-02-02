module Error exposing
    ( Error
    , Reason(..)
    , validateBool
    , validateChoice
    , validateColor
    , validateDate
    , validateFile
    , validateFloat
    , validateInt
    , validateStories
    , validateString
    , validateTime
    , view
    )

import AVL.Dict as Dict exposing (Dict)
import Color exposing (Color)
import Css
import Date exposing (Time)
import Html.Styled as Html exposing (Html, br, code, div, pre, span, styled, text)
import Knob
import Palette
import Renderer exposing (Renderer)
import Story exposing (Story)
import String.Format
import SyntaxHighlight
import Time
import Utils exposing (ifelse, nonBlank)


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
    | EmptyRadio String
    | EmptySelect String
    | DuplicateRadioOptions String String Int
    | DuplicateSelectOptions String String Int
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


type alias Error =
    { path : Story.Path
    , reason : Reason
    }


validateOnlyKnobName : String -> Result Reason { name : String }
validateOnlyKnobName rawName =
    case nonBlank rawName of
        Nothing ->
            Err EmptyKnobTitle

        Just name ->
            Ok { name = name }


validateString : String -> Result Reason { name : String }
validateString =
    validateOnlyKnobName


validateBool : String -> Result Reason { name : String }
validateBool =
    validateOnlyKnobName


validateFile : String -> Result Reason { name : String }
validateFile =
    validateOnlyKnobName


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


validateInt : String -> Int -> Knob.Limits Int -> Result (List Reason) { name : String }
validateInt rawName int limits =
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case validateIntLimits name int limits of
                [] ->
                    Ok { name = name }

                reasons ->
                    Err reasons


validateFloat : String -> Float -> Knob.Limits Float -> Result (List Reason) { name : String }
validateFloat rawName float limits =
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case validateFloatLimits name float limits of
                [] ->
                    Ok { name = name }

                reasons ->
                    Err reasons


validateChoice : Knob.Choice -> String -> List ( String, option ) -> Result (List Reason) { name : String, selected : String, option : option }
validateChoice choice rawName options =
    let
        ( emptyReason, duplicateReason ) =
            case choice of
                Knob.Radio ->
                    ( EmptyRadio, DuplicateRadioOptions )

                Knob.Select ->
                    ( EmptySelect, DuplicateSelectOptions )
    in
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case List.head options of
                Nothing ->
                    Err [ emptyReason name ]

                Just ( selected, option ) ->
                    case
                        List.map
                            (\( value, n ) -> duplicateReason name value n)
                            (countList Tuple.first options)
                    of
                        [] ->
                            Ok { name = name, selected = selected, option = option }

                        duplicateReasons ->
                            Err duplicateReasons


validateColor : String -> String -> Result Reason { name : String, color : Color }
validateColor rawName value =
    case ( nonBlank rawName, Color.fromString value ) of
        ( Nothing, _ ) ->
            Err EmptyKnobTitle

        ( Just name, Nothing ) ->
            Err (InvalidColor name value)

        ( Just name, Just color ) ->
            Ok { name = name, color = color }


validateDate : String -> String -> Result Reason { name : String, date : Time.Posix }
validateDate rawName value =
    case ( nonBlank rawName, Date.parseStringToPosix value ) of
        ( Nothing, _ ) ->
            Err EmptyKnobTitle

        ( Just name, Nothing ) ->
            Err (InvalidDate name value)

        ( Just name, Just date ) ->
            Ok { name = name, date = date }


validateTime : String -> String -> Result Reason { name : String, time : Time }
validateTime rawName value =
    case ( nonBlank rawName, Date.timeFromString value ) of
        ( Nothing, _ ) ->
            Err EmptyKnobTitle

        ( Just name, Nothing ) ->
            Err (InvalidTime name value)

        ( Just name, Just time ) ->
            Ok { name = name, time = time }


type alias Counter =
    { counts : Dict String Int
    , duplicates : List String
    }


initialCounter : Counter
initialCounter =
    Counter Dict.empty []


count : String -> Counter -> Counter
count title { counts, duplicates } =
    case Dict.get title counts of
        Nothing ->
            Counter (Dict.insert title 1 counts) duplicates

        Just 1 ->
            Counter (Dict.insert title 2 counts) (title :: duplicates)

        Just n ->
            Counter (Dict.insert title (n + 1) counts) duplicates


incount : Counter -> List ( String, Int )
incount { counts, duplicates } =
    List.filterMap
        (\key -> Maybe.map (Tuple.pair key) (Dict.get key counts))
        duplicates


countList : (a -> String) -> List a -> List ( String, Int )
countList toKey list =
    incount (List.foldr (count << toKey) initialCounter list)


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
    FolderCounters initialCounter initialCounter initialCounter


validateStory : Story.Path -> Story Reason Renderer -> FolderCounters -> ( Result (List Error) (Story Never Renderer), FolderCounters )
validateStory path story counters =
    case story of
        Story.Label rawTitle ->
            case nonBlank rawTitle of
                Nothing ->
                    ( Err [ Error (List.reverse path) EmptyLabelTitle ]
                    , counters
                    )

                Just title ->
                    ( Ok (Story.Label title)
                    , { counters | labels = count title counters.labels }
                    )

        Story.Todo rawTitle ->
            case nonBlank rawTitle of
                Nothing ->
                    ( Err [ Error (List.reverse path) EmptyTodoTitle ]
                    , counters
                    )

                Just title ->
                    ( Ok (Story.Todo title)
                    , { counters | stories = count title counters.stories }
                    )

        Story.Single rawTitle payload ->
            case nonBlank rawTitle of
                Nothing ->
                    ( Err [ Error (List.reverse path) EmptyStoryTitle ]
                    , counters
                    )

                Just title ->
                    ( case
                        countList Tuple.first payload.knobs
                            |> List.map (\( name, n ) -> DuplicateKnobs name n)
                            |> List.map (Error (List.reverse (title :: path)))
                      of
                        [] ->
                            Ok (Story.Single title payload)

                        errors ->
                            Err errors
                    , { counters | stories = count title counters.stories }
                    )

        Story.Fail rawTitle reasons ->
            case nonBlank rawTitle of
                Nothing ->
                    ( Err [ Error (List.reverse path) EmptyStoryTitle ]
                    , counters
                    )

                Just title ->
                    ( Err (List.map (Error (List.reverse (title :: path))) reasons)
                    , { counters | stories = count title counters.stories }
                    )

        Story.Batch rawTitle substories ->
            case nonBlank rawTitle of
                Nothing ->
                    ( Err [ Error (List.reverse path) EmptyFolderTitle ]
                    , counters
                    )

                Just title ->
                    ( Result.map (Story.Batch title) (validateStories (title :: path) substories)
                    , { counters | folders = count title counters.folders }
                    )


validateStories : Story.Path -> List (Story Reason Renderer) -> Result (List Error) (List (Story Never Renderer))
validateStories path stories =
    let
        ( result, counters ) =
            List.foldr
                (\story ( acc, folderCounters ) ->
                    let
                        ( validateion, nextCounters ) =
                            validateStory path story folderCounters
                    in
                    case ( validateion, acc ) of
                        ( Err errors, Err errors_ ) ->
                            ( Err (errors ++ errors_)
                            , nextCounters
                            )

                        ( Err errors, Ok _ ) ->
                            ( Err errors
                            , nextCounters
                            )

                        ( Ok _, Err errors ) ->
                            ( Err errors
                            , nextCounters
                            )

                        ( Ok nextStory, Ok nextStories ) ->
                            ( Ok (nextStory :: nextStories)
                            , nextCounters
                            )
                )
                ( Ok [], initialFolderCounters )
                stories
    in
    case ( result, incountFolder (List.reverse path) counters ) of
        ( _, [] ) ->
            result

        ( Err errors, errors_ ) ->
            Err (errors ++ errors_)

        ( _, errors ) ->
            Err errors


type alias Explanation msg =
    { label : List (Html msg)
    , description : String
    , code : String
    , diffs : List ( SyntaxHighlight.Highlight, Int, Int )
    }


viewMark : String -> Html msg
viewMark =
    styled span
        [ Css.display Css.inlineBlock
        , Css.padding2 (Css.px 2) (Css.px 4)
        , Css.borderRadius (Css.px 3)
        , Css.backgroundColor Palette.gray
        , Css.color Palette.white
        , Css.letterSpacing (Css.em 0.05)
        ]
        []
        << List.singleton
        << text


viewCodeMark : String -> Html msg
viewCodeMark =
    styled code
        [ Css.display Css.inlineBlock
        , Css.padding2 (Css.px 2) (Css.px 4)
        , Css.backgroundColor Palette.cloud
        , Css.letterSpacing (Css.em 0.05)
        , Css.fontFamily Css.monospace
        ]
        []
        << List.singleton
        << text


reasonEmptyTitle : String
reasonEmptyTitle =
    "Please make sure you've defined neither empty or blank title."


reasonEmptyLabelTitle : Explanation msg
reasonEmptyLabelTitle =
    Explanation
        [ text "Label with empty title"
        ]
        reasonEmptyTitle
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


reasonEmptyTodoTitle : Explanation msg
reasonEmptyTodoTitle =
    Explanation
        [ text "Todo with empty title"
        ]
        reasonEmptyTitle
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


reasonEmptyStoryTitle : Explanation msg
reasonEmptyStoryTitle =
    Explanation
        [ text "Story with empty title"
        ]
        reasonEmptyTitle
        """
[ Bulletproof.storyOf ""
[ Bulletproof.storyOf "Not empty title"
    (burgerIcon
        |> Bulletproof.fromHtml
    )

--
, Bulletproof.todo "   "
, Bulletproof.todo "Not blank title"
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


reasonEmptyFolderTitle : Explanation msg
reasonEmptyFolderTitle =
    Explanation
        [ text "Folder with empty title"
        ]
        reasonEmptyTitle
        """
[ Bulletproof.folderOf "" []
[ Bulletproof.folderOf "Not empty title" []

--
, Bulletproof.folderOf "   " []
, Bulletproof.folderOf "Not blank title" []
]
        """
        [ ( SyntaxHighlight.Del, 0, 1 )
        , ( SyntaxHighlight.Add, 1, 2 )
        , ( SyntaxHighlight.Del, 4, 5 )
        , ( SyntaxHighlight.Add, 5, 6 )
        ]


reasonDuplicateLabels : String -> Int -> Explanation msg
reasonDuplicateLabels title n =
    Explanation
        [ text "Label "
        , viewMark title
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        "Please make sure you've defined unique titles for lables inside a folder."
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


reasonDuplicateStories : String -> Int -> Explanation msg
reasonDuplicateStories title n =
    Explanation
        [ text "Story/Todo "
        , viewMark title
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        """
Please make sure you've defined unique titles for both stories and todos inside a folder.
Each todo is a story which has not started yet...
        """
        """
[ Bulletproof.todo "Icon"
[ Bulletproof.todo "Icon times"

--
, Bulletproof.todo "Icon"
, Bulletproof.todo "Icon user"

--
, Bulletproof.storyOf "Icon"
, Bulletproof.storyOf "Icon burger"
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


reasonDuplicateFolders : String -> Int -> Explanation msg
reasonDuplicateFolders title n =
    Explanation
        [ text "Folder "
        , viewMark title
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        "Please make sure you've defined unique titles for folders inside a folder."
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


reasonEmptyKnobTitle : Explanation msg
reasonEmptyKnobTitle =
    Explanation
        [ text "Knob with empty title"
        ]
        reasonEmptyTitle
        """
Bulletproof.storyOf "Button"
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


reasonDuplicateKnobs : String -> Int -> Explanation msg
reasonDuplicateKnobs name n =
    Explanation
        [ text "Knob "
        , viewMark name
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        "Please make sure you've defined unique names for knobs inside a story."
        """
Bulletproof.storyOf "Button"
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


reasonEmptyChoice : String -> String -> Explanation msg
reasonEmptyChoice knobName name =
    Explanation
        [ viewCodeMark ("Knob." ++ knobName ++ " \"" ++ name ++ "\" ")
        , text " has no options to chooce"
        ]
        "Please make sure you've defined at least single option"
        ("""
Bulletproof.storyOf "Button"
    (\\buttonType ->
        button
            [ type_ buttonType
            ]
            [ text "Funny Button"
            ]
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.{{ knob }} "Button type"
        []
        [ ( "button", "button" )
        , ( "reset", "reset" )
        , ( "submit", "submit" )
        ]
        """
            |> String.Format.namedValue "knob" knobName
        )
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 15 )
        ]


reasonDuplicateChoice : String -> String -> String -> Int -> Explanation msg
reasonDuplicateChoice knobName name option n =
    Explanation
        [ viewCodeMark ("Knob." ++ knobName ++ " \"" ++ name ++ "\" ")
        , text " option "
        , viewMark option
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        "Please make sure you've defined unique names of options."
        ("""
Bulletproof.storyOf "Input"
    (\\inputType ->
        input
            [ type_ inputType
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.{{ knob }} "Input type"
        [ ( "string", "text" )
        [ ( "plain text", "text" )
        , ( "string", "email" )
        , ( "user email", "email" )
        , ( "string", "password" )
        , ( "user password", "password" )
        ]
        """
            |> String.Format.namedValue "knob" knobName
        )
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
        , ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        , ( SyntaxHighlight.Del, 13, 14 )
        , ( SyntaxHighlight.Add, 14, 15 )
        ]


reasonInvalidIntStep : String -> Int -> Explanation msg
reasonInvalidIntStep name step =
    Explanation
        [ viewCodeMark ("Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , viewCodeMark ("Knob.step " ++ String.fromInt step)
        ]
        "Please make sure you've defined a positive step."
        """
Bulletproof.storyOf "Input"
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


reasonInvalidFloatStep : String -> Float -> Explanation msg
reasonInvalidFloatStep name step =
    Explanation
        [ viewCodeMark ("Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , viewCodeMark ("Knob.step " ++ String.fromFloat step)
        ]
        "Please make sure you've defined a positive step."
        """
Bulletproof.storyOf "Progressbar"
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


reasonInvalidIntMin : String -> Int -> Int -> Explanation msg
reasonInvalidIntMin name value min =
    Explanation
        [ viewCodeMark ("Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.min " ++ String.fromInt min)
        , text " more than actual value "
        , viewCodeMark (String.fromInt value)
        ]
        "Please make sure you've defined min boundary less or equal to value."
        """
Bulletproof.storyOf "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        50
        [ Bulletproof.Knob.min 100
        [ Bulletproof.Knob.min 25
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
        """
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        ]


reasonInvalidFloatMin : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMin name value min =
    Explanation
        [ viewCodeMark ("Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.min " ++ String.fromFloat min)
        , text " more than actual value "
        , viewCodeMark (String.fromFloat value)
        ]
        "Please make sure you've defined min boundary less or equal to value."
        """
Bulletproof.storyOf "Progressbar"
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


reasonInvalidIntMax : String -> Int -> Int -> Explanation msg
reasonInvalidIntMax name value max =
    Explanation
        [ viewCodeMark ("Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.max " ++ String.fromInt max)
        , text " less than actual value "
        , viewCodeMark (String.fromInt value)
        ]
        "Please make sure you've defined max boundary more or equal to value."
        """
Bulletproof.storyOf "Input"
    (\\inputSize ->
        input
            [ size inputSize
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.int "Input size"
        500
        [ Bulletproof.Knob.min 25
        , Bulletproof.Knob.max 200
        , Bulletproof.Knob.max 1000
        , Bulletproof.Knob.step 10
        ]
        """
        [ ( SyntaxHighlight.Del, 11, 12 )
        , ( SyntaxHighlight.Add, 12, 13 )
        ]


reasonInvalidFloatMax : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMax name value max =
    Explanation
        [ viewCodeMark ("Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.max " ++ String.fromFloat max)
        , text " less than actual value "
        , viewCodeMark (String.fromFloat value)
        ]
        "Please make sure you've defined min boundary less or equal to value."
        """
Bulletproof.storyOf "Progressbar"
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


reasonInvalidIntMinMax : String -> Int -> Int -> Explanation msg
reasonInvalidIntMinMax name value max =
    Explanation
        [ viewCodeMark ("Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.min " ++ String.fromInt max)
        , text " less than "
        , viewCodeMark ("Knob.max" ++ String.fromInt value)
        ]
        "Please make sure you've defined min boundary less or equal to max boundary."
        """
Bulletproof.storyOf "Input"
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


reasonInvalidFloatMinMax : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMinMax name value max =
    Explanation
        [ viewCodeMark ("Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , viewCodeMark ("Knob.min " ++ String.fromFloat max)
        , text " less than "
        , viewCodeMark ("Knob.max" ++ String.fromFloat value)
        ]
        "Please make sure you've defined min boundary less or equal to max boundary."
        """
Bulletproof.storyOf "Progressbar"
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


reasonInvalidColor : String -> String -> Explanation msg
reasonInvalidColor name color =
    Explanation
        [ viewCodeMark ("Knob.color" ++ " \"" ++ name ++ "\" ")
        , text " has invalid color "
        , viewCodeMark color
        ]
        "Please make sure provided value is following regular hex color pattern \"#rgb\" or \"#rrggbb\"."
        """
Bulletproof.storyOf "Colored Button"
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


reasonInvalidDate : String -> String -> Explanation msg
reasonInvalidDate name date =
    Explanation
        [ viewCodeMark ("Knob.date" ++ " \"" ++ name ++ "\" ")
        , text " has invalid date "
        , viewCodeMark date
        ]
        "Please make sure provided value is following one of the \"dd-mm-yyyy\" or \"dd/mm/yyyy\" date patterns."
        """
Bulletproof.storyOf "Date show"
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


reasonInvalidTime : String -> String -> Explanation msg
reasonInvalidTime name time =
    Explanation
        [ viewCodeMark ("Knob.time" ++ " \"" ++ name ++ "\" ")
        , text " has invalid time "
        , viewCodeMark time
        ]
        "Please make sure provided value is following the \"mm:hh\" time pattern."
        """
Bulletproof.storyOf "Time show"
    (\\time ->
        div
            []
            [ text ("Minutes " ++ String.fromInt time.minutes)
            , text (", Hours " ++ String.fromInt time.hours)
            ]
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.time "Show time" "32-13-2020"
    |> Bulletproof.Knob.time "Show time" "02-02-2020"
        """
        [ ( SyntaxHighlight.Del, 9, 10 )
        , ( SyntaxHighlight.Add, 10, 11 )
        ]


reasonToExplanation : Reason -> Explanation msg
reasonToExplanation reason =
    case reason of
        EmptyLabelTitle ->
            reasonEmptyLabelTitle

        EmptyTodoTitle ->
            reasonEmptyTodoTitle

        EmptyStoryTitle ->
            reasonEmptyStoryTitle

        EmptyFolderTitle ->
            reasonEmptyFolderTitle

        DuplicateLabels title n ->
            reasonDuplicateLabels title n

        DuplicateStories title n ->
            reasonDuplicateStories title n

        DuplicateFolders title n ->
            reasonDuplicateFolders title n

        EmptyKnobTitle ->
            reasonEmptyKnobTitle

        DuplicateKnobs name n ->
            reasonDuplicateKnobs name n

        EmptyRadio name ->
            reasonEmptyChoice "radio" name

        EmptySelect name ->
            reasonEmptyChoice "select" name

        DuplicateRadioOptions name option n ->
            reasonDuplicateChoice "radio" name option n

        DuplicateSelectOptions name option n ->
            reasonDuplicateChoice "select" name option n

        InvalidIntStep name step ->
            reasonInvalidIntStep name step

        InvalidFloatStep name step ->
            reasonInvalidFloatStep name step

        InvalidIntMin name value min ->
            reasonInvalidIntMin name value min

        InvalidFloatMin name value min ->
            reasonInvalidFloatMin name value min

        InvalidIntMax name value max ->
            reasonInvalidIntMax name value max

        InvalidFloatMax name value max ->
            reasonInvalidFloatMax name value max

        InvalidIntMinMax name min max ->
            reasonInvalidIntMinMax name min max

        InvalidFloatMinMax name min max ->
            reasonInvalidFloatMinMax name min max

        InvalidColor name color ->
            reasonInvalidColor name color

        InvalidDate name date ->
            reasonInvalidDate name date

        InvalidTime name time ->
            reasonInvalidTime name time


styledLabel : List (Html msg) -> Html msg
styledLabel =
    styled div
        [ Css.marginBottom (Css.px 4)
        , Css.fontWeight Css.bold
        , Css.fontSize (Css.px 16)
        ]
        []


styledDescription : List (Html msg) -> Html msg
styledDescription =
    styled div
        [ Css.marginTop (Css.px 12)
        , Css.fontSize (Css.px 13)
        ]
        []


viewDescription : String -> Html msg
viewDescription description =
    description
        |> String.trim
        |> String.split "\n"
        |> List.map (text << String.trim)
        |> List.intersperse (br [] [])
        |> styledDescription


styledPath : List (Html msg) -> Html msg
styledPath =
    styled code
        [ Css.padding2 (Css.px 2) (Css.px 4)
        , Css.backgroundColor Palette.smoke
        , Css.color Palette.gray
        , Css.borderRadius (Css.px 3)
        , Css.fontFamily Css.monospace
        , Css.fontSize (Css.px 10)
        ]
        []


viewPath : List String -> Html msg
viewPath path =
    styledPath
        [ if List.isEmpty path then
            text "/"

          else
            text (String.join " / " ("" :: path))
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
        [ Css.padding3 (Css.px 12) (Css.px 16) (Css.px 8)
        , Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
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
        , viewDescription explanation.description
        , viewCodeExample explanation.code explanation.diffs
        ]


styledContainer : List (Html msg) -> Html msg
styledContainer =
    styled div
        [ Css.margin2 (Css.px 8) Css.zero
        , Css.width (Css.px 680)
        , Css.maxWidth (Css.pct 100)
        , Css.backgroundColor Palette.white
        , Css.boxShadow4 Css.zero Css.zero (Css.px 10) Palette.smoke
        ]
        []


styledRoot : List (Html msg) -> Html msg
styledRoot =
    styled div
        [ Css.boxSizing Css.borderBox
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.center
        , Css.flex3 (Css.int 1) (Css.int 1) Css.zero
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
