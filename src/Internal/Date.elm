module Internal.Date exposing
    ( Date
    , Month
    , Posix
    , Weekday
    , decoder
    , fromInt
    , fromString
    , parse
    , toString
    )

import DateTime exposing (DateTime)
import Json.Decode as Decode exposing (Decoder)
import Time


type alias Posix =
    Time.Posix


type alias Month =
    Time.Month


type alias Weekday =
    Time.Weekday


type alias Date =
    { posix : Posix
    , year : Int
    , month : Month
    , monthIndex : Int
    , day : Int
    , weekday : Weekday
    , weekdayIndex : Int
    }


fromInt : Int -> Date
fromInt int =
    let
        posix =
            Time.millisToPosix int

        month =
            Time.toMonth Time.utc posix

        weekday =
            Time.toWeekday Time.utc posix
    in
    Date posix
        (Time.toYear Time.utc posix)
        month
        (monthToIndex month)
        (Time.toDay Time.utc posix)
        weekday
        (weekdayToIndex weekday)


decoder : Decoder Date
decoder =
    Decode.map fromInt Decode.int


monthFromIndex : Int -> Maybe Month
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


monthToIndex : Month -> Int
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


weekdayToIndex : Weekday -> Int
weekdayToIndex weekday =
    case weekday of
        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6

        Time.Sun ->
            7


toDateTime : Int -> Int -> Month -> Maybe DateTime
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


parse : String -> Maybe Date
parse str =
    List.foldl
        (\delimiter result ->
            case result of
                Nothing ->
                    Maybe.map (fromInt << DateTime.toMillis) (parseWithDelimiter delimiter str)

                just ->
                    just
        )
        Nothing
        [ '-', '/' ]


fromString : String -> Maybe Date
fromString str =
    case List.map String.toInt (String.split "-" str) of
        [ Just year, Just monthIndex, Just day ] ->
            monthFromIndex monthIndex
                |> Maybe.andThen (toDateTime day year)
                |> Maybe.map (fromInt << DateTime.toMillis)

        _ ->
            Nothing


toString : Date -> String
toString date =
    String.join "-"
        [ String.padLeft 4 '0' (String.fromInt date.year)
        , String.padLeft 2 '0' (String.fromInt (monthToIndex date.month))
        , String.padLeft 2 '0' (String.fromInt date.day)
        ]
