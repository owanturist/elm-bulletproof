module Date exposing
    ( Date
    , Time
    , dateFromString
    , dateToString
    , timeFromString
    , timeToString
    )

import Calendar
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


dateFromDateTime : DateTime -> Date
dateFromDateTime datetime =
    Date
        (DateTime.toPosix datetime)
        (DateTime.getYear datetime)
        (Calendar.monthToInt (DateTime.getMonth datetime))
        (DateTime.getDay datetime)


charsToInt : List Char -> Maybe Int
charsToInt =
    String.toInt << String.fromList


charsToMonth : List Char -> Maybe Time.Month
charsToMonth =
    Maybe.andThen (\monthIndex -> Dict.get monthIndex indexToMonthTable) << charsToInt


makeDateTime : List Char -> List Char -> List Char -> Maybe DateTime
makeDateTime day month year =
    Maybe.andThen
        (\rawDate ->
            DateTime.fromRawParts
                rawDate
                { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }
        )
        (Maybe.map3 Calendar.RawDate
            (charsToInt year)
            (charsToMonth month)
            (charsToInt day)
        )


parseStringToDateTimeHelp : String -> Char -> Maybe DateTime
parseStringToDateTimeHelp str delimiter =
    case List.map String.toList (String.split (String.fromChar delimiter) str) of
        [ [ _, _, _, _ ] as yyyy, [ _, _ ] as mm, [ _, _ ] as dd ] ->
            makeDateTime dd mm yyyy

        [ [ _, _ ] as dd, [ _, _ ] as mm, [ _, _, _, _ ] as yyyy ] ->
            makeDateTime dd mm yyyy

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


validateTime : Time -> Maybe Time
validateTime time =
    if time.hours >= 0 && time.hours < 24 && time.minutes >= 0 && time.minutes < 60 then
        Just time

    else
        Nothing


timeFromString : String -> Maybe Time
timeFromString str =
    case String.toList str of
        [ h0, h1, ':', m0, m1 ] ->
            Maybe.map2
                Time
                (charsToInt [ h0, h1 ])
                (charsToInt [ m0, m1 ])
                |> Maybe.andThen validateTime

        _ ->
            Nothing


timeToString : Time -> String
timeToString time =
    String.join ":"
        [ String.padLeft 2 '0' (String.fromInt time.hours)
        , String.padLeft 2 '0' (String.fromInt time.minutes)
        ]
