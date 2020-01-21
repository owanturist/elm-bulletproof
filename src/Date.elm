module Date exposing
    ( Date
    , Time
    , dateFromPosix
    , parseStringToPosix
    , posixFromString
    , posixToString
    , timeFromString
    , timeToString
    )

import DateTime exposing (DateTime)
import Time



-- D A T E


type alias Date =
    { posix : Time.Posix
    , year : Int
    , month : Int
    , day : Int
    }


monthFromIndex : Int -> Maybe Time.Month
monthFromIndex index =
    case index of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


monthToIndex : Time.Month -> Int
monthToIndex month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


toDateTime : Int -> Int -> Time.Month -> Maybe DateTime
toDateTime day year month =
    DateTime.fromRawParts
        { day = day, month = month, year = year }
        { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }


parseWithDelimiter : Char -> String -> Maybe DateTime
parseWithDelimiter delimiter str =
    case
        str
            |> String.split (String.fromChar delimiter)
            |> List.map String.toInt
    of
        [ Just day, Just monthIndex, Just year ] ->
            Maybe.andThen (toDateTime day year) (monthFromIndex monthIndex)

        _ ->
            Nothing


parseStringToPosix : String -> Maybe Time.Posix
parseStringToPosix str =
    List.foldl
        (\delimiter result ->
            case result of
                Nothing ->
                    Maybe.map DateTime.toPosix (parseWithDelimiter delimiter str)

                just ->
                    just
        )
        Nothing
        [ '-', '/' ]


dateFromPosix : Time.Posix -> Date
dateFromPosix posix =
    Date posix
        (Time.toYear Time.utc posix)
        (monthToIndex (Time.toMonth Time.utc posix))
        (Time.toDay Time.utc posix)


posixFromString : String -> Maybe Time.Posix
posixFromString str =
    case List.map String.toInt (String.split "-" str) of
        [ Just year, Just monthIndex, Just day ] ->
            monthFromIndex monthIndex
                |> Maybe.andThen (toDateTime day year)
                |> Maybe.map DateTime.toPosix

        _ ->
            Nothing


posixToString : Time.Posix -> String
posixToString posix =
    String.join "-"
        [ Time.toYear Time.utc posix
            |> String.fromInt
            |> String.padLeft 4 '0'
        , Time.toMonth Time.utc posix
            |> monthToIndex
            |> String.fromInt
            |> String.padLeft 2 '0'
        , Time.toDay Time.utc posix
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]



-- T I M E


type alias Time =
    { hours : Int
    , minutes : Int
    }


timeFromString : String -> Maybe Time
timeFromString str =
    case List.map String.toInt (String.split ":" str) of
        [ Just hours, Just minutes ] ->
            if hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60 then
                Just (Time hours minutes)

            else
                Nothing

        _ ->
            Nothing


timeToString : Time -> String
timeToString time =
    String.join ":"
        [ String.padLeft 2 '0' (String.fromInt time.hours)
        , String.padLeft 2 '0' (String.fromInt time.minutes)
        ]
