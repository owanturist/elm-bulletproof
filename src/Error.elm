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
import Html.Styled as Html exposing (Html, br, code, div, pre, styled, text)
import Html.Styled.Attributes as Attributes
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
    | EmptyChoice Knob.Choice String
    | EmptyChoiceOption Knob.Choice String
    | DuplicateChoiceOptions Knob.Choice String String Int
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


validateOnlyKnobName : String -> Result Reason String
validateOnlyKnobName rawName =
    case nonBlank rawName of
        Nothing ->
            Err EmptyKnobTitle

        Just name ->
            Ok name


validateString : String -> Result Reason String
validateString =
    validateOnlyKnobName


validateBool : String -> Result Reason String
validateBool =
    validateOnlyKnobName


validateFile : String -> Result Reason String
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


validateInt : String -> Int -> Knob.Limits Int -> Result (List Reason) String
validateInt rawName int limits =
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case validateIntLimits name int limits of
                [] ->
                    Ok name

                reasons ->
                    Err reasons


validateFloat : String -> Float -> Knob.Limits Float -> Result (List Reason) String
validateFloat rawName float limits =
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case validateFloatLimits name float limits of
                [] ->
                    Ok name

                reasons ->
                    Err reasons


validateChoice : Knob.Choice -> String -> List ( String, option ) -> Result (List Reason) { name : String, selected : String, option : option }
validateChoice choice rawName options =
    case nonBlank rawName of
        Nothing ->
            Err [ EmptyKnobTitle ]

        Just name ->
            case List.map (Tuple.mapFirst String.trim) options of
                [] ->
                    Err [ EmptyChoice choice name ]

                (( selected, option ) :: _) as trimmedOptions ->
                    if List.member "" (List.map Tuple.first trimmedOptions) then
                        Err [ EmptyChoiceOption choice name ]

                    else
                        case
                            List.map
                                (\( value, n ) -> DuplicateChoiceOptions choice name value n)
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


textCode : String -> Html msg
textCode =
    styled code
        [ Css.display Css.inlineBlock
        , Css.padding2 (Css.px 2) (Css.px 4)
        , Css.backgroundColor Palette.cloud
        , Css.letterSpacing (Css.em 0.05)
        , Css.fontFamily Css.monospace
        , Css.lineHeight (Css.px 18)
        ]
        []
        << List.singleton
        << text


reasonEmptyLabelTitle : Explanation msg
reasonEmptyLabelTitle =
    Explanation
        [ textCode "Bulletproof.label"
        , text " has either empty or blank title"
        ]
        "Please make sure you've defined neither empty or blank title."
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
        [ textCode "Bulletproof.todo"
        , text " has either empty or blank title"
        ]
        "Please make sure you've defined neither empty or blank title."
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
        [ textCode "Bulletproof.story"
        , text " has either empty or blank title"
        ]
        "Please make sure you've defined neither empty or blank title."
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


reasonEmptyFolderTitle : Explanation msg
reasonEmptyFolderTitle =
    Explanation
        [ textCode "Bulletproof.folder"
        , text " has either empty or blank title"
        ]
        "Please make sure you've defined neither empty or blank title."
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


reasonDuplicateLabels : String -> Int -> Explanation msg
reasonDuplicateLabels title n =
    Explanation
        [ textCode ("Bulletproof.label \"" ++ title ++ "\"")
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
        [ textCode ("Bulletproof.story \"" ++ title ++ "\"")
        , text " or "
        , textCode ("Bulletproof.todo \"" ++ title ++ "\"")
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


reasonDuplicateFolders : String -> Int -> Explanation msg
reasonDuplicateFolders title n =
    Explanation
        [ textCode ("Bulletproof.folder \"" ++ title ++ "\"")
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
        [ textCode "Bulletproof.Knob.*"
        , text " has either empty or blank name"
        ]
        "Please make sure you've defined neither empty or blank name."
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


reasonDuplicateKnobs : String -> Int -> Explanation msg
reasonDuplicateKnobs name n =
    Explanation
        [ textCode ("Bulletproof.Knob.* \"" ++ name ++ "\"")
        , text (" repeats " ++ String.fromInt n ++ " times")
        ]
        "Please make sure you've defined unique names for knobs inside a story."
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


choiceToString : Knob.Choice -> String
choiceToString choice =
    case choice of
        Knob.Radio ->
            "radio"

        Knob.Select ->
            "select"


reasonEmptyChoice : Knob.Choice -> String -> Explanation msg
reasonEmptyChoice choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choiceToString choice ++ " \"" ++ name ++ "\" ")
        , text " has no options"
        ]
        "Please make sure you've defined neither empty or blank options."
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
    |> Bulletproof.Knob.{{ knob }} "Button type"
        []
        [ ( "button", "button" )
        , ( "reset", "reset" )
        , ( "submit", "submit" )
        ]
        """
            |> String.Format.namedValue "knob" (choiceToString choice)
        )
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 15 )
        ]


reasonEmptyChoiceOption : Knob.Choice -> String -> Explanation msg
reasonEmptyChoiceOption choice name =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choiceToString choice ++ " \"" ++ name ++ "\" ")
        , text " has either empty or blank options"
        ]
        "Please make sure you've defined neither empty or blank options."
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
    |> Bulletproof.Knob.{{ knob }} "Input value"
        [ ( "", "" )
        [ ( "empty string", "" )
        , ( "   ", "   " )
        , ( "blank string", "   " )
        , ( "short string", "Hello World!" )
        , ( "long string", "Lorem ipsum dolor..." )
        ]
        """
            |> String.Format.namedValue "knob" (choiceToString choice)
        )
        [ ( SyntaxHighlight.Del, 10, 11 )
        , ( SyntaxHighlight.Add, 11, 12 )
        , ( SyntaxHighlight.Del, 12, 13 )
        , ( SyntaxHighlight.Add, 13, 14 )
        ]


reasonDuplicateChoiceOptions : Knob.Choice -> String -> String -> Int -> Explanation msg
reasonDuplicateChoiceOptions choice name option n =
    Explanation
        [ textCode ("Bulletproof.Knob." ++ choiceToString choice ++ " \"" ++ name ++ "\" ")
        , text (" has " ++ String.fromInt n ++ "\u{00A0}times repeated option ")
        , textCode ("\"" ++ option ++ "\"")
        ]
        "Please make sure you've defined unique names of options."
        ("""
Bulletproof.story "Input"
    (\\inputType ->
        input
            [ type_ inputType
            ]
            []
            |> Bulletproof.fromHtml
    )
    |> Bulletproof.Knob.{{ knob }} "Input type"
        [ ( "string", "text" )
        , ( "string", "email" )
        , ( "string", "password" )
        [ ( "plain text", "text" )
        , ( "user email", "email" )
        , ( "user password", "password" )
        ]
        """
            |> String.Format.namedValue "knob" (choiceToString choice)
        )
        [ ( SyntaxHighlight.Del, 9, 12 )
        , ( SyntaxHighlight.Add, 12, 15 )
        ]


reasonInvalidIntStep : String -> Int -> Explanation msg
reasonInvalidIntStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromInt step)
        ]
        "Please make sure you've defined a positive step."
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


reasonInvalidFloatStep : String -> Float -> Explanation msg
reasonInvalidFloatStep name step =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has not positive "
        , textCode ("Bulletproof.Knob.step " ++ String.fromFloat step)
        ]
        "Please make sure you've defined a positive step."
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


reasonInvalidIntMin : String -> Int -> Int -> Explanation msg
reasonInvalidIntMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " more than actual value"
        ]
        "Please make sure the min boundary is less or equal to the value."
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


reasonInvalidFloatMin : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMin name value min =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " more than actual value"
        ]
        "Please make sure the min boundary is less or equal to the value."
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


reasonInvalidIntMax : String -> Int -> Int -> Explanation msg
reasonInvalidIntMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" " ++ String.fromInt value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        , text " less than actual value"
        ]
        "Please make sure the max boundary is more or equal to the value."
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


reasonInvalidFloatMax : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMax name value max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" " ++ String.fromFloat value)
        , text " has "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        , text " less than actual value"
        ]
        "Please make sure the max boundary is more or equal to the value."
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


reasonInvalidIntMinMax : String -> Int -> Int -> Explanation msg
reasonInvalidIntMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.int" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromInt min)
        , text " more than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromInt max)
        ]
        "Please make sure the min boundary less or equal to the max boundary."
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


reasonInvalidFloatMinMax : String -> Float -> Float -> Explanation msg
reasonInvalidFloatMinMax name min max =
    Explanation
        [ textCode ("Bulletproof.Knob.float" ++ " \"" ++ name ++ "\" ")
        , text " has "
        , textCode ("Bulletproof.Knob.min " ++ String.fromFloat min)
        , text " more than "
        , textCode ("Bulletproof.Knob.max " ++ String.fromFloat max)
        ]
        "Please make sure the min boundary less or equal to the max boundary."
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


reasonInvalidColor : String -> String -> Explanation msg
reasonInvalidColor name color =
    Explanation
        [ textCode ("Bulletproof.Knob.color" ++ " \"" ++ name ++ "\" \"" ++ color ++ "\"")
        , text " has invalid color"
        ]
        "Please make sure provided value is following hex color pattern \"#rgb\" or \"#rrggbb\"."
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


reasonInvalidDate : String -> String -> Explanation msg
reasonInvalidDate name date =
    Explanation
        [ textCode ("Bulletproof.Knob.date" ++ " \"" ++ name ++ "\" \"" ++ date ++ "\"")
        , text " has invalid date"
        ]
        "Please make sure provided value is following one of the \"dd-mm-yyyy\" or \"dd/mm/yyyy\" date patterns."
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


reasonInvalidTime : String -> String -> Explanation msg
reasonInvalidTime name time =
    Explanation
        [ textCode ("Bulletproof.Knob.time" ++ " \"" ++ name ++ "\" \"" ++ time ++ "\"")
        , text " has invalid time "
        ]
        "Please make sure provided value is following the \"mm:hh\" time pattern."
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

        EmptyChoice choice name ->
            reasonEmptyChoice choice name

        EmptyChoiceOption choice name ->
            reasonEmptyChoiceOption choice name

        DuplicateChoiceOptions choice name option n ->
            reasonDuplicateChoiceOptions choice name option n

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
        , Css.fontSize (Css.px 14)
        , Css.lineHeight (Css.px 24)
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
        , viewDescription explanation.description
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
