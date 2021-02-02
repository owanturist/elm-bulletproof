module Date exposing
    ( Date
    , Time
    , dateFromString
    , dateToString
    , timeFromString
    , timeToString
    )

import DateTime exposing (DateTime)
import Dict exposing (Dict)
import Html.Attributes exposing (datetime)
import Time



-- D A T E


type alias Date =
    { posix : Time.Posix
    , year : Int
    , month : Int
    , day : Int
    }


indexToMonthTable : Dict Int Time.Month
indexToMonthTable =
    [ ( 1, Time.Jan )
    , ( 2, Time.Feb )
    , ( 3, Time.Mar )
    , ( 4, Time.Apr )
    , ( 5, Time.May )
    , ( 6, Time.Jun )
    , ( 7, Time.Jul )
    , ( 8, Time.Aug )
    , ( 9, Time.Sep )
    , ( 10, Time.Oct )
    , ( 11, Time.Nov )
    , ( 12, Time.Dec )
    ]
        |> Dict.fromList


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


dateFromDateTime : DateTime -> Date
dateFromDateTime datetime =
    Date
        (DateTime.toPosix datetime)
        (DateTime.getYear datetime)
        (monthToIndex (DateTime.getMonth datetime))
        (DateTime.getDay datetime)


makeDateTime : Int -> Int -> Int -> Maybe DateTime
makeDateTime day monthIndex year =
    Maybe.andThen
        (\month ->
            DateTime.fromRawParts
                { day = day, month = month, year = year }
                { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }
        )
        (Dict.get monthIndex indexToMonthTable)


parseStringToDateTimeHelp : String -> Char -> Maybe DateTime
parseStringToDateTimeHelp str delimiter =
    case List.map String.toInt (String.split (String.fromChar delimiter) str) of
        [ Just yearOrDay, Just monthIndex, Just dayOrYear ] ->
            [ makeDateTime dayOrYear monthIndex yearOrDay
            , makeDateTime yearOrDay monthIndex dayOrYear
            ]
                |> List.filterMap identity
                |> List.head

        _ ->
            Nothing


parseStringToDateTime : String -> Maybe DateTime
parseStringToDateTime str =
    [ '-', '/', '.' ]
        |> List.filterMap (parseStringToDateTimeHelp str)
        |> List.head


dateFromString : String -> Maybe Date
dateFromString =
    Maybe.map dateFromDateTime << parseStringToDateTime


dateToString : Date -> String
dateToString date =
    String.join "-"
        [ date.year
            |> String.fromInt
            |> String.padLeft 4 '0'
        , date.month
            |> String.fromInt
            |> String.padLeft 2 '0'
        , date.day
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
