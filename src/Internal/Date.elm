module Internal.Date exposing
    ( Date
    , fromPosix
    , fromString
    , toPosix
    , toString
    )

import DateTime exposing (DateTime)
import Time


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


toPosix : String -> Maybe Time.Posix
toPosix str =
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


fromPosix : Time.Posix -> Date
fromPosix posix =
    Date posix
        (Time.toYear Time.utc posix)
        (monthToIndex (Time.toMonth Time.utc posix))
        (Time.toDay Time.utc posix)


fromString : String -> Maybe Time.Posix
fromString str =
    case List.map String.toInt (String.split "-" str) of
        [ Just year, Just monthIndex, Just day ] ->
            monthFromIndex monthIndex
                |> Maybe.andThen (toDateTime day year)
                |> Maybe.map DateTime.toPosix

        _ ->
            Nothing


toString : Time.Posix -> String
toString posix =
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
