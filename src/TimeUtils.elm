module TimeUtils exposing
    ( ContinuousTimes
    , toContinuousTimes
    , toDayOrdinal
    , toDaysInMonth
    , toMonthString
    , toWeekdayString
    )

import Time exposing (..)



-- Each unit of time as a continous time starting from zero to their respective
-- maximum
--
-- eg: 1 minute, 30 seconds, 250 milliseconds in continuous seconds will be 30.25


type alias ContinuousTimes =
    { seconds : Float
    , minutes : Float
    , hours : Float
    , days : Float
    , weekdays : Float
    , months : Float
    }


toContinuousTimes : Zone -> Posix -> ContinuousTimes
toContinuousTimes zone posix =
    let
        milliseconds =
            toFloat (toMillis zone posix)

        seconds =
            toFloat (toSecond zone posix) + (milliseconds / 1000)

        minutes =
            toFloat (toMinute zone posix) + (seconds / 60)

        hours =
            toFloat (toHour zone posix) + (minutes / 60)

        days =
            toFloat (toDay zone posix) - 1 + (hours / 24)

        weekdays =
            toFloat (toWeekdayInt zone posix) + (hours / 24)

        months =
            toFloat (toMonthInt zone posix) + (days / toFloat (toDaysInMonth zone posix))
    in
    { seconds = seconds
    , minutes = minutes
    , hours = hours
    , days = days
    , weekdays = weekdays
    , months = months
    }



-- Returns true if the given date/time is in a leap year


isLeapYear : Zone -> Posix -> Bool
isLeapYear zone posix =
    let
        year =
            toYear zone posix
    in
    (modBy 400 year == 0) || (modBy 100 year /= 0) && (modBy 4 year == 0)



-- Returns the weekday as an Int with Monday as 0


toWeekdayInt : Zone -> Posix -> Int
toWeekdayInt zone posix =
    case toWeekday zone posix of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6



-- Returns the month as an Int with January as 0


toMonthInt : Zone -> Posix -> Int
toMonthInt zone posix =
    case toMonth zone posix of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11



-- Returns the number of days in the month of the given date/time


toDaysInMonth : Zone -> Posix -> Int
toDaysInMonth zone posix =
    case toMonth zone posix of
        Jan ->
            31

        Feb ->
            if isLeapYear zone posix then
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



-- Returns the weekday as a String


toWeekdayString : Zone -> Posix -> String
toWeekdayString zone posix =
    case toWeekday zone posix of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"



-- Returns the month as a String


toMonthString : Zone -> Posix -> String
toMonthString zone posix =
    case toMonth zone posix of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"



-- Returns the day as an ordinal number
-- Eg: 1st, 2nd, 3rd, etc


toDayOrdinal : Zone -> Posix -> String
toDayOrdinal zone posix =
    let
        day =
            toDay zone posix

        suffix =
            if day == 1 || day == 21 || day == 31 then
                "st"

            else if day == 2 || day == 22 then
                "nd"

            else if day == 3 || day == 23 then
                "rd"

            else
                "th"
    in
    String.fromInt day ++ suffix
