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
import Html.Styled as Html exposing (Html, code, div, pre, styled, text)
import Knob
import Palette
import Renderer exposing (Renderer)
import Story exposing (Story)
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
    | DuplicateKnob String Int
    | EmptyRadio String
    | EmptySelect String
    | DuplicateRadioOptions String String Int
    | DuplicateSelectOptions String String Int
    | InvalidIntStep String Int
    | InvalidIntLeftBoundary String Int Int
    | InvalidIntRightBoundary String Int Int
    | InvalidIntBoundaries String Int Int
    | InvalidFloatStep String Float
    | InvalidFloatLeftBoundary String Float Float
    | InvalidFloatRightBoundary String Float Float
    | InvalidFloatBoundaries String Float Float
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
    , Maybe.andThen (\( min, max ) -> ifelse (min < max) Nothing (Just (onBoth name min max))) (Maybe.map2 Tuple.pair limits.min limits.max)
    ]
        |> List.filterMap identity


validateIntLimits : String -> Int -> Knob.Limits Int -> List Reason
validateIntLimits =
    validateLimits
        { onStep = InvalidIntStep
        , onLeft = InvalidIntLeftBoundary
        , onRight = InvalidIntRightBoundary
        , onBoth = InvalidIntBoundaries
        }


validateFloatLimits : String -> Float -> Knob.Limits Float -> List Reason
validateFloatLimits =
    validateLimits
        { onStep = InvalidFloatStep
        , onLeft = InvalidFloatLeftBoundary
        , onRight = InvalidFloatRightBoundary
        , onBoth = InvalidFloatBoundaries
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
                            |> List.map (\( name, n ) -> DuplicateKnob name n)
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


reasonEmptyLabelTitle : ( String, String, String )
reasonEmptyLabelTitle =
    ( "Empty label"
    , "Please make sure you've defined neither empty or blank lablels' title."
    , """
Bulletproof.label "Label example"
    """
    )


reasonToExplanation : Reason -> ( String, String, String )
reasonToExplanation reason =
    case reason of
        EmptyLabelTitle ->
            reasonEmptyLabelTitle

        _ ->
            ( "", "", "" )


viewReason : Reason -> Html msg
viewReason reason =
    case reason of
        EmptyLabelTitle ->
            text "EmptyLabelTitle"

        EmptyStoryTitle ->
            text "EmptyStoryTitle"

        EmptyTodoTitle ->
            text "EmptyTodoTitle"

        EmptyFolderTitle ->
            text "EmptyFolderTitle"

        EmptyKnobTitle ->
            text "EmptyKnobTitle"

        DuplicateLabels title n ->
            text ("DuplicateLabels `" ++ title ++ "` " ++ String.fromInt n ++ " times")

        DuplicateStories title n ->
            text ("DuplicateStories `" ++ title ++ "` " ++ String.fromInt n ++ " times")

        DuplicateFolders title n ->
            text ("DuplicateFolders `" ++ title ++ "` " ++ String.fromInt n ++ " times")

        DuplicateKnob name n ->
            text ("DuplicateKnob `" ++ name ++ "` " ++ String.fromInt n ++ " times")

        EmptyRadio name ->
            text ("EmptyRadio `" ++ name ++ "`")

        EmptySelect name ->
            text ("EmptySelect `" ++ name ++ "`")

        DuplicateRadioOptions name option n ->
            text ("DuplicateRadioOptions `" ++ name ++ "`: `" ++ option ++ "` " ++ String.fromInt n ++ " times")

        DuplicateSelectOptions name option n ->
            text ("DuplicateSelectOptions `" ++ name ++ "`: `" ++ option ++ "` " ++ String.fromInt n ++ " times")

        InvalidIntStep name step ->
            text ("InvalidIntStep `" ++ name ++ "` " ++ String.fromInt step ++ " <= 0")

        InvalidIntLeftBoundary name value min ->
            text ("InvalidIntLeftBoundary `" ++ name ++ "` " ++ String.fromInt value ++ " < " ++ String.fromInt min)

        InvalidIntRightBoundary name value max ->
            text ("InvalidIntRightBoundary `" ++ name ++ "` " ++ String.fromInt value ++ " > " ++ String.fromInt max)

        InvalidIntBoundaries name min max ->
            text ("InvalidIntBoundaries `" ++ name ++ "` " ++ String.fromInt min ++ " > " ++ String.fromInt max)

        InvalidFloatStep name step ->
            text ("InvalidFloatStep `" ++ name ++ "` " ++ String.fromFloat step ++ " <= 0")

        InvalidFloatLeftBoundary name value min ->
            text ("InvalidFloatLeftBoundary `" ++ name ++ "` " ++ String.fromFloat value ++ " < " ++ String.fromFloat min)

        InvalidFloatRightBoundary name value max ->
            text ("InvalidFloatRightBoundary `" ++ name ++ "` " ++ String.fromFloat value ++ " > " ++ String.fromFloat max)

        InvalidFloatBoundaries name min max ->
            text ("InvalidFloatBoundaries `" ++ name ++ "` " ++ String.fromFloat min ++ " > " ++ String.fromFloat max)

        InvalidColor name color ->
            text ("InvalidColor `" ++ name ++ "`: `" ++ color ++ "`")

        InvalidDate name date ->
            text ("InvalidDate `" ++ name ++ "`: `" ++ date ++ "`")

        InvalidTime name time ->
            text ("InvalidTime `" ++ name ++ "`: `" ++ time ++ "`")


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
        [ Css.marginTop (Css.px 8)
        , Css.fontSize (Css.px 13)
        ]
        []


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


styledError : List (Html msg) -> Html msg
styledError =
    styled div
        [ Css.padding (Css.px 16)
        , Css.borderBottom3 (Css.px 1) Css.solid Palette.smoke
        ]
        []


viewCodeExample : String -> Html msg
viewCodeExample exampleCode =
    case SyntaxHighlight.elm (String.trim exampleCode) of
        Err _ ->
            pre [] [ text (String.trim exampleCode) ]

        Ok elmCode ->
            Html.fromUnstyled (SyntaxHighlight.toBlockHtml Nothing elmCode)


viewError : Error -> Html msg
viewError error =
    let
        ( label, description, codeExample ) =
            reasonToExplanation error.reason
    in
    styledError
        [ styledLabel [ text label ]
        , viewPath error.path
        , styledDescription [ text description ]
        , viewCodeExample codeExample
        ]


styledContainer : List (Html msg) -> Html msg
styledContainer =
    styled div
        [ Css.width (Css.px 600)
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
