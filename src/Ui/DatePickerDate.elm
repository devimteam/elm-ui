module Ui.DatePickerDate
    exposing
        ( YearRange(..)
        , initDate
        , formatDate
        , formatDateMonthFullName
        , formatDay
        , formatMonth
        , addDays
        , addDows
        , subDays
        , dateTuple
        , datesInRange
        , firstOfMonth
        , prevMonth
        , nextMonth
        , newYear
        , yearRange
        , formatCalendarHeaderDate
        , fromString
        )

import Date exposing (Date, Day(..), Month(..), year, month, day)
import Regex


type alias Year =
    Int


type alias Day =
    Int


type YearRange
    = Off
    | MoreOrLess Int
    | Between Year Year
    | From Year
    | To Year


distruct : String -> ( String, String, String )
distruct text =
    let
        addDot d =
            if String.length d > 1 then
                d ++ "."
            else
                d

        day =
            String.left 2 text

        month =
            (String.dropLeft 2 >> String.left 2) text

        year =
            (String.dropLeft 4 >> String.left 4) text
    in
        ( addDot day, addDot month, year )


fromString : String -> Result String Date
fromString text_ =
    let
        numerize =
            Regex.replace Regex.All (Regex.regex "[^0-9]") (\_ -> "")

        text =
            numerize text_

        parsed ( day, month, year ) =
            month ++ day ++ year
    in
        Date.fromString <| parsed <| distruct text


initDate : Date
initDate =
    mkDate 1992 May 29


formatDate : Date -> String
formatDate date =
    dayToString (day date) ++ "." ++ monthToString (month date) ++ "." ++ toString (year date)


formatDateMonthFullName : Date -> String
formatDateMonthFullName date =
    dayToString (day date) ++ " " ++ formatMonth2 (month date) ++ " " ++ toString (year date)


formatCalendarHeaderDate : Date -> String
formatCalendarHeaderDate date =
    formatDay (Date.dayOfWeek date)
        ++ ", "
        ++ dayToString (day date)
        ++ " "
        ++ formatMonth1 (month date)
        ++ "."


formatDay : Date.Day -> String
formatDay day =
    case day of
        Mon ->
            "Пн"

        Tue ->
            "Вт"

        Wed ->
            "Ср"

        Thu ->
            "Чт"

        Fri ->
            "Пт"

        Sat ->
            "Сб"

        Sun ->
            "Вск"


formatMonth2 : Month -> String
formatMonth2 month =
    case month of
        Jan ->
            "января"

        Feb ->
            "февраля"

        Mar ->
            "марта"

        Apr ->
            "апреля"

        May ->
            "мая"

        Jun ->
            "июня"

        Jul ->
            "июля"

        Aug ->
            "августа"

        Sep ->
            "сентября"

        Oct ->
            "октября"

        Nov ->
            "ноября"

        Dec ->
            "декабря"


formatMonth1 : Month -> String
formatMonth1 month =
    case month of
        Jan ->
            "янв"

        Feb ->
            "фев"

        Mar ->
            "мар"

        Apr ->
            "апр"

        May ->
            "май"

        Jun ->
            "июн"

        Jul ->
            "июл"

        Aug ->
            "авг"

        Sep ->
            "сент"

        Oct ->
            "окт"

        Nov ->
            "ноя"

        Dec ->
            "дек"


formatMonth : Month -> String
formatMonth month =
    case month of
        Jan ->
            "Январь"

        Feb ->
            "Февраль"

        Mar ->
            "Март"

        Apr ->
            "Апрель"

        May ->
            "Май"

        Jun ->
            "Июнь"

        Jul ->
            "Июль"

        Aug ->
            "Август"

        Sep ->
            "Сентябрь"

        Oct ->
            "Октябрь"

        Nov ->
            "Ноябрь"

        Dec ->
            "Декабрь"


trimDates : Date.Day -> List Date -> List Date
trimDates firstDay dates =
    let
        lastDay =
            predDow firstDay

        dl dates =
            case dates of
                [] ->
                    []

                x :: xs ->
                    if Date.dayOfWeek x == firstDay then
                        dates
                    else
                        dl xs

        dr dates =
            case dates of
                [] ->
                    []

                x :: xs ->
                    if Date.dayOfWeek x == lastDay then
                        dates
                    else
                        dr xs
    in
        dl dates
            |> List.reverse
            |> dr
            |> List.reverse


datesInRange : Date.Day -> Date -> Date -> List Date
datesInRange firstDay min max =
    let
        go x acc =
            let
                y =
                    subDay x
            in
                if dateTuple y == dateTuple min then
                    y :: acc
                else
                    go y (y :: acc)
    in
        go max []
            |> trimDates firstDay


dateTuple : Date -> ( Int, Int, Int )
dateTuple date =
    ( year date, monthToInt <| month date, day date )


repeat : (a -> a) -> Int -> a -> a
repeat f =
    let
        go n x =
            if n == 0 then
                x
            else
                go (n - 1) (f x)
    in
        go


firstOfMonth : Date -> Date
firstOfMonth date =
    mkDate (year date) (month date) 1


nextMonth : Date -> Date
nextMonth date =
    let
        nextMonth =
            succMonth (month date)

        nextYear =
            if nextMonth == Jan then
                year date + 1
            else
                year date
    in
        mkDate nextYear nextMonth 1


prevMonth : Date -> Date
prevMonth date =
    let
        prevMonth =
            predMonth (month date)

        prevYear =
            if prevMonth == Dec then
                year date - 1
            else
                year date
    in
        mkDate prevYear prevMonth 1


addDays : Int -> Date -> Date
addDays =
    repeat addDay


addDay : Date -> Date
addDay date =
    let
        month =
            Date.month date

        year =
            Date.year date

        dim =
            daysInMonth year month

        day =
            Date.day date + 1

        succ =
            succMonth month

        succYear =
            if succ == Jan then
                year + 1
            else
                year
    in
        if day > dim then
            mkDate succYear succ 1
        else
            mkDate year month day


subDays : Int -> Date -> Date
subDays =
    repeat subDay


subDay : Date -> Date
subDay date =
    let
        month =
            Date.month date

        year =
            Date.year date

        day =
            Date.day date - 1

        pred =
            predMonth month

        predYear =
            if pred == Dec then
                year - 1
            else
                year
    in
        if day < 1 then
            mkDate predYear pred (daysInMonth predYear pred)
        else
            mkDate year month day


addDows : Int -> Date.Day -> Date.Day
addDows =
    repeat succDow


succDow : Date.Day -> Date.Day
succDow day =
    dayToInt day
        |> flip rem 7
        |> (+) 1
        |> dayFromInt


subDows : Int -> Date.Day -> Date.Day
subDows =
    repeat succDow


predDow : Date.Day -> Date.Day
predDow day =
    let
        prev =
            (dayToInt day - 1)
                |> flip rem 7
    in
        if prev == 0 then
            Sun
        else
            dayFromInt prev


dayToString : Int -> String
dayToString day =
    if day < 10 then
        "0" ++ toString day
    else
        toString day


dayToInt : Date.Day -> Int
dayToInt day =
    case day of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7


dayFromInt : Int -> Date.Day
dayFromInt day =
    case day of
        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        6 ->
            Sat

        7 ->
            Sun

        _ ->
            Debug.crash ("dayFromInt: invalid day: " ++ toString day)


monthToString : Month -> String
monthToString month =
    let
        int =
            monthToInt month
    in
        if int < 10 then
            "0" ++ toString int
        else
            toString int


predMonth : Month -> Month
predMonth month =
    let
        prev =
            (monthToInt month - 1)
                |> flip rem 12
    in
        if prev == 0 then
            Dec
        else
            monthFromInt prev


succMonth : Month -> Month
succMonth month =
    monthToInt month
        |> flip rem 12
        |> (+) 1
        |> monthFromInt


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthFromInt : Int -> Month
monthFromInt month =
    case month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        12 ->
            Dec

        x ->
            Debug.crash ("monthFromInt: invalid month: " ++ toString x)


daysInMonth : Year -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Year -> Bool
isLeapYear y =
    y % 400 == 0 || y % 100 /= 0 && y % 4 == 0


mkDate : Year -> Month -> Day -> Date
mkDate year month day =
    toString year
        ++ "/"
        ++ monthToString month
        ++ "/"
        ++ dayToString day
        |> unsafeDate


unsafeDate : String -> Date
unsafeDate date =
    case Date.fromString date of
        Err e ->
            Debug.crash ("unsafeDate: failed to parse date:" ++ e)

        Ok date ->
            date


newYear : Date -> String -> Date
newYear currentMonth newYear =
    case String.toInt newYear of
        Ok year ->
            mkDate year (month currentMonth) (day currentMonth)

        Err _ ->
            Debug.crash ("Unknown Month " ++ (toString currentMonth))


yearRange : { focused : Date, currentMonth : Date } -> YearRange -> List Int
yearRange { focused, currentMonth } range =
    case range of
        MoreOrLess num ->
            List.range ((year currentMonth) - num) ((year currentMonth) + num)

        Between start end ->
            List.range start end

        From year_ ->
            List.range year_ (year focused)

        To year_ ->
            List.range (year focused) year_

        Off ->
            []
