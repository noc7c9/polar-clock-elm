module PolarClock exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgUtils
import Time
import TimeUtils
import Utils exposing (Coord, hslToString)


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

        toAngle : Int -> Float -> Float
        toAngle max value =
            2 * pi * (value / toFloat max)

        continuous =
            TimeUtils.toContinuousTimes zone posix

        secondAngle =
            toAngle 60 continuous.seconds

        minuteAngle =
            toAngle 60 continuous.minutes

        hourAngle =
            toAngle 24 continuous.hours

        weekdayAngle =
            toAngle 7 continuous.weekdays

        monthdayAngle =
            toAngle (TimeUtils.toDaysInMonth zone posix) continuous.days

        monthAngle =
            toAngle 12 continuous.months

        toLabel value name =
            let
                intValue =
                    floor value

                plural =
                    if intValue == 1 then
                        ""

                    else
                        "s"
            in
            String.fromInt intValue ++ " " ++ name ++ plural

        secondLabel =
            toLabel continuous.seconds "second"

        minuteLabel =
            toLabel continuous.minutes "minute"

        hourLabel =
            toLabel continuous.hours "hour"

        weekdayLabel =
            TimeUtils.toWeekdayString zone posix

        monthdayLabel =
            TimeUtils.toDayOrdinal zone posix

        monthLabel =
            TimeUtils.toMonthString zone posix

        yearLabel =
            Time.toYear zone posix |> String.fromInt

        outerRadius index =
            50
                - settings.margin
                - (index * (settings.arcWidth + settings.gapWidth))

        innerRadius index =
            outerRadius index - settings.arcWidth
    in
    svg
        [ viewBox "0 0 100 100" ]
        [ arc "seconds" center secondLabel 0 (innerRadius 0) (outerRadius 0) secondAngle
        , arc "minutes" center minuteLabel 60 (innerRadius 1) (outerRadius 1) minuteAngle
        , arc "hours" center hourLabel 120 (innerRadius 2) (outerRadius 2) hourAngle
        , arc "day-of-week" center weekdayLabel 180 (innerRadius 3) (outerRadius 3) weekdayAngle
        , arc "day-of-month" center monthdayLabel 240 (innerRadius 4) (outerRadius 4) monthdayAngle
        , arc "month" center monthLabel 300 (innerRadius 5) (outerRadius 5) monthAngle
        , arc "year" center yearLabel 360 0.1 (outerRadius 6) (2 * pi - 0.0000001)
        ]


arc : String -> Coord -> String -> Float -> Float -> Float -> Float -> Html msg
arc id center label hueOffset innerRadius outerRadius angle =
    let
        color =
            hslToString (hueOffset + angle) 0.8 0.5

        fontSize =
            "2.4px"

        textPad =
            0.65

        centerRadius =
            (innerRadius + outerRadius) / 2

        textFlipped =
            angle < (pi / 2) || angle > (3 * pi / 2)

        textPadRadius =
            textPad / centerRadius
    in
    g []
        [ -- black circle stroke in the background
          Svg.path
            [ fill "none"
            , stroke "black"
            , d (SvgUtils.arcLinePath center centerRadius 0 (2 * pi - 0.0000001) True)
            ]
            []

        -- The arc
        , Svg.path
            [ fill color
            , stroke "none"
            , d (SvgUtils.arcShapePath center innerRadius outerRadius 0 angle 0.6)
            ]
            []

        -- The path for the label text
        , Svg.path
            [ Html.Attributes.id id
            , fill "none"
            , stroke "none"
            , d
                (SvgUtils.arcLinePath center
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

        -- The label text
        , Svg.text_
            [ Svg.Attributes.fontSize fontSize
            , fontFamily "'Lato', sans-serif"
            ]
            [ Svg.textPath
                [ xlinkHref ("#" ++ id)
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
                [ Svg.text label ]
            ]
        ]
