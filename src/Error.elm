module Error exposing
    ( Error
    , Reason(..)
    , validateChoice
    , validateColor
    , validateDate
    , validateFloat
    , validateInt
    , validateNameOnly
    , validateTime
    )

import Color exposing (Color)
import Date exposing (Time)
import Knob
import Time
import Utils exposing (ifelse, nonBlank)


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
    | EmptySelect String
    | DuplicateRadio String (List String)
    | DuplicateSelect String (List String)
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
    { path : List String
    , reason : Reason
    }


validateNameOnly : String -> Result Reason { name : String }
validateNameOnly rawName =
    case nonBlank rawName of
        Nothing ->
            Err EmptyKnobTitle

        Just name ->
            Ok { name = name }


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


validateChoice : Knob.Choice -> String -> List ( String, option ) -> Result Reason { name : String, selected : String, option : option }
validateChoice choice rawName options =
    case ( nonBlank rawName, List.head options ) of
        ( Nothing, _ ) ->
            Err EmptyKnobTitle

        ( Just name, Nothing ) ->
            case choice of
                Knob.Radio ->
                    Err (EmptyRadio name)

                Knob.Select ->
                    Err (EmptySelect name)

        ( Just name, Just ( selected, option ) ) ->
            Ok { name = name, selected = selected, option = option }


validateColor : String -> String -> Result Reason { name : String, color : Color }
validateColor rawName value =
    case ( nonBlank rawName, Color.fromString value ) of
        ( Nothing, _ ) ->
            Err EmptyKnobTitle

        ( Just name, Nothing ) ->
            Err (InvalidDate name value)

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
