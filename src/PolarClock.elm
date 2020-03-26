module PolarClock exposing (..)

import Arc exposing (Coord, arc)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type alias Settings =
    { arcWidth : Float
    , gapWidth : Float
    , margin : Float
    }


polarClock : Settings -> Time.Zone -> Time.Posix -> Html msg
polarClock settings zone posix =
    let
        center =
            Coord 50 50

        toAngle : Float -> Float -> Float
        toAngle max value =
            2 * pi * (value / max)

        milliseconds =
            toFloat (Time.toMillis zone posix)

        seconds =
            toFloat (Time.toSecond zone posix)
                + (milliseconds / 1000)

        minutes =
            toFloat (Time.toMinute zone posix)
                + (seconds / 60)

        hours =
            toFloat (Time.toHour zone posix)
                + (minutes / 60)

        monthday =
            toFloat (Time.toDay zone posix)
                - 1
                + (hours / 24)

        weekday =
            let
                value =
                    case Time.toWeekday zone posix of
                        Time.Mon ->
                            0

                        Time.Tue ->
                            1

                        Time.Wed ->
                            2

                        Time.Thu ->
                            3

                        Time.Fri ->
                            4

                        Time.Sat ->
                            5

                        Time.Sun ->
                            6
            in
            toFloat value + (hours / 24)

        isLeapYear =
            let
                yearInt =
                    Time.toYear zone posix
            in
            (modBy 400 yearInt == 0)
                || (modBy 100 yearInt /= 0)
                && (modBy 4 yearInt == 0)

        daysInMonth =
            case Time.toMonth zone posix of
                Time.Jan ->
                    31

                Time.Feb ->
                    if isLeapYear then
                        29

                    else
                        28

                Time.Mar ->
                    31

                Time.Apr ->
                    30

                Time.May ->
                    31

                Time.Jun ->
                    30

                Time.Jul ->
                    31

                Time.Aug ->
                    31

                Time.Sep ->
                    30

                Time.Oct ->
                    31

                Time.Nov ->
                    30

                Time.Dec ->
                    31

        month =
            let
                value =
                    case Time.toMonth zone posix of
                        Time.Jan ->
                            0

                        Time.Feb ->
                            1

                        Time.Mar ->
                            2

                        Time.Apr ->
                            3

                        Time.May ->
                            4

                        Time.Jun ->
                            5

                        Time.Jul ->
                            6

                        Time.Aug ->
                            7

                        Time.Sep ->
                            8

                        Time.Oct ->
                            9

                        Time.Nov ->
                            10

                        Time.Dec ->
                            11
            in
            toFloat value + (monthday / daysInMonth)

        secondAngle =
            toAngle 60 seconds

        minuteAngle =
            toAngle 60 minutes

        hourAngle =
            toAngle 24 hours

        weekdayAngle =
            toAngle 7 weekday

        monthdayAngle =
            toAngle daysInMonth monthday

        monthAngle =
            toAngle 12 month

        outerRadius index =
            50
                - settings.margin
                - (index * (settings.arcWidth + settings.gapWidth))

        innerRadius index =
            outerRadius index - settings.arcWidth
    in
    svg
        [ viewBox "0 0 100 100" ]
        [ viewArc center "seconds" "red" (innerRadius 0) (outerRadius 0) secondAngle
        , viewArc center "minutes" "green" (innerRadius 1) (outerRadius 1) minuteAngle
        , viewArc center "hours" "blue" (innerRadius 2) (outerRadius 2) hourAngle
        , viewArc center "day-of-week" "yellow" (innerRadius 3) (outerRadius 3) weekdayAngle
        , viewArc center "day-of-month" "yellow" (innerRadius 4) (outerRadius 4) monthdayAngle
        , viewArc center "month" "yellow" (innerRadius 5) (outerRadius 5) monthAngle
        ]


viewArc : Coord -> String -> String -> Float -> Float -> Float -> Html msg
viewArc center name color innerRadius outerRadius angle =
    let
        arcSettings =
            { center = center
            , innerRadius = innerRadius
            , outerRadius = outerRadius
            , startAngle = 0
            , endAngle = angle
            , borderRadius = 0.6
            }

        arcAttrs =
            -- [ id name, fill "none", strokeWidth "0.2", stroke color ]
            [ id name, fill color, stroke "none" ]

        textAttrs =
            [ fontSize "2.4", fontFamily "'Lato', sans-serif" ]
    in
    g []
        [ arc arcAttrs arcSettings
        , Svg.text_ textAttrs
            [ Svg.textPath [ xlinkHref ("#" ++ name), startOffset "0" ]
                [ Svg.text (name ++ ": " ++ String.fromInt (floor angle)) ]
            ]
        ]


viewArcUnRounded : Coord -> String -> String -> Float -> Float -> Float -> Html msg
viewArcUnRounded center name color innerRadius outerRadius angle =
    let
        arcSettings =
            { center = center
            , innerRadius = innerRadius
            , outerRadius = outerRadius
            , startAngle = 0
            , endAngle = angle
            , borderRadius = 0
            }

        arcAttrs =
            -- [ id name, fill "none", strokeWidth "0.2", stroke color ]
            [ id name, fill color, stroke "none" ]

        textAttrs =
            [ fontSize "2.4", fontFamily "'Lato', sans-serif" ]
    in
    g []
        [ arc arcAttrs arcSettings
        , Svg.text_ textAttrs
            [ Svg.textPath [ xlinkHref ("#" ++ name), startOffset "0" ]
                [ Svg.text (name ++ ": " ++ String.fromInt (floor angle)) ]
            ]
        ]
