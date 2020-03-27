module PolarClock exposing (..)

import Html exposing (Html)
import Html.Attributes
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
        fontSize =
            "2.4px"

        textPad =
            0.5

        centerRadius =
            (innerRadius + outerRadius) / 2

        circle =
            Svg.path
                [ fill "none"
                , stroke "black"
                , d (arcPath center centerRadius 0 (2 * pi - 0.0000001) True)
                ]
                []

        arc =
            Svg.path
                [ fill color
                , stroke "none"
                , d (arcShape center innerRadius outerRadius 0 angle 0.6)
                ]
                []

        textFlipped =
            angle < (pi / 2) || angle > (3 * pi / 2)

        textPadRadius =
            textPad / centerRadius

        textArc =
            Svg.path
                [ id name
                , fill "none"
                , stroke "none"
                , d
                    (arcPath center
                        (if textFlipped then
                            centerRadius

                         else
                            centerRadius + 1.5
                        )
                        textPadRadius
                        (angle - textPadRadius)
                        (not textFlipped)
                    )
                ]
                []
    in
    g []
        [ circle
        , arc
        , textArc
        , Svg.text_
            [ Svg.Attributes.fontSize fontSize
            , fontFamily "'Lato', sans-serif"
            ]
            [ Svg.textPath
                [ xlinkHref ("#" ++ name)
                , startOffset
                    (if textFlipped then
                        "100%"

                     else
                        "0%"
                    )
                , textAnchor
                    (if textFlipped then
                        "end"

                     else
                        "start"
                    )
                ]
                [ Svg.text (name ++ ": " ++ String.fromInt (floor angle)) ]
            ]
        ]


type alias Coord =
    { x : Float, y : Float }


arcPath : Coord -> Float -> Float -> Float -> Bool -> String
arcPath center radius startAngle endAngle reverse =
    let
        start =
            polarToCartesian center radius startAngle

        end =
            polarToCartesian center radius endAngle

        largeArc =
            (endAngle - startAngle) > pi
    in
    if reverse then
        String.join " "
            [ moveTo end
            , arcTo radius radius start largeArc False
            ]

    else
        String.join " "
            [ moveTo start
            , arcTo radius radius end largeArc True
            ]


arcShape : Coord -> Float -> Float -> Float -> Float -> Float -> String
arcShape center innerRadius outerRadius startAngle endAngle rawBorderRadius =
    let
        borderRadius =
            -- Clamp to half the stroke width so the arc ends are half-circles
            Basics.min rawBorderRadius ((outerRadius - innerRadius) / 2)

        outerBorderRadiusAngle =
            borderRadius / outerRadius

        innerBorderRadiusAngle =
            borderRadius / innerRadius

        innerStartAngle =
            startAngle + innerBorderRadiusAngle

        innerEndAngle =
            endAngle - innerBorderRadiusAngle

        innerStart =
            polarToCartesian center innerRadius innerStartAngle

        innerCenterStart =
            polarToCartesian center (innerRadius + borderRadius) startAngle

        innerEnd =
            polarToCartesian center innerRadius innerEndAngle

        innerCenterEnd =
            polarToCartesian center (innerRadius + borderRadius) endAngle

        outerStartAngle =
            startAngle + outerBorderRadiusAngle

        outerEndAngle =
            endAngle - outerBorderRadiusAngle

        outerStart =
            polarToCartesian center outerRadius outerStartAngle

        outerCenterStart =
            polarToCartesian center (outerRadius - borderRadius) startAngle

        outerEnd =
            polarToCartesian center outerRadius outerEndAngle

        outerCenterEnd =
            polarToCartesian center (outerRadius - borderRadius) endAngle

        innerLargeArc =
            (innerEndAngle - innerStartAngle) > pi

        outerLargeArc =
            (outerEndAngle - outerStartAngle) > pi
    in
    String.join " "
        [ moveTo innerCenterStart
        , arcTo borderRadius borderRadius innerStart False False
        , arcTo innerRadius innerRadius innerEnd innerLargeArc True
        , arcTo borderRadius borderRadius innerCenterEnd False False
        , lineTo outerCenterEnd
        , arcTo borderRadius borderRadius outerEnd False False
        , arcTo outerRadius outerRadius outerStart outerLargeArc False
        , arcTo borderRadius borderRadius outerCenterStart False False
        , join
        ]


moveTo : Coord -> String
moveTo coord =
    "M " ++ String.fromFloat coord.x ++ "," ++ String.fromFloat coord.y


lineTo : Coord -> String
lineTo coord =
    "L " ++ String.fromFloat coord.x ++ "," ++ String.fromFloat coord.y


arcTo : Float -> Float -> Coord -> Bool -> Bool -> String
arcTo rx ry end largeArc sweepFlag =
    let
        boolToString value =
            if value then
                "1"

            else
                "0"
    in
    String.join " "
        [ "A"
        , String.fromFloat rx
        , ","
        , String.fromFloat ry
        , "0"
        , boolToString largeArc
        , boolToString sweepFlag
        , String.fromFloat end.x
        , String.fromFloat end.y
        ]


join : String
join =
    "Z"


polarToCartesian : Coord -> Float -> Float -> Coord
polarToCartesian center radius angle =
    { x = center.x + radius * sin angle
    , y = center.y - radius * cos angle
    }
