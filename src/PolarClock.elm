module PolarClock exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgUtils
import Time
import TimeUtils
import Utils exposing (Coord, hslToString)


type alias HSL =
    { hue : Float
    , saturation : Float
    , lightness : Float
    }


type alias Settings =
    { arcWidth : Float
    , gapWidth : Float
    , margin : Float
    , colorInitial : Float
    , colorDifference : Float
    , saturation : Float
    , lightness : Float
    , fontFamily : String
    , fontSize : String
    , textPosition : Float
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

        color index =
            { hue = settings.colorInitial + (index * settings.colorDifference)
            , saturation = settings.saturation
            , lightness = settings.lightness
            }

        textAttrs =
            [ fontSize settings.fontSize
            , fontFamily settings.fontFamily
            ]

        parameterisedArc index id label angle =
            let
                inner =
                    innerRadius index

                outer =
                    outerRadius index

                textPosition =
                    Basics.max 0.0 (Basics.min 1.0 settings.textPosition)

                textRadius =
                    (inner * textPosition) + (outer * (1 - textPosition))
            in
            arc id center label textRadius textAttrs (color index) inner outer angle
    in
    svg
        [ viewBox "0 0 100 100" ]
        [ parameterisedArc 0 "seconds" secondLabel secondAngle
        , parameterisedArc 1 "minutes" minuteLabel minuteAngle
        , parameterisedArc 2 "hours" hourLabel hourAngle
        , parameterisedArc 3 "day-of-week" weekdayLabel weekdayAngle
        , parameterisedArc 4 "day-of-month" monthdayLabel monthdayAngle
        , parameterisedArc 5 "month" monthLabel monthAngle
        , circle "year" center yearLabel textAttrs (color 6) (outerRadius 6)
        ]


arc : String -> Coord -> String -> Float -> List (Html.Attribute msg) -> HSL -> Float -> Float -> Float -> Html msg
arc id center label textRadius textAttrs hsl innerRadius outerRadius angle =
    let
        color =
            hslToString (hsl.hue + angle) hsl.saturation hsl.lightness

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
                (SvgUtils.arcLinePath center textRadius textPadRadius (angle - textPadRadius) (not textFlipped))
            ]
            []

        -- The label text
        , Svg.text_
            textAttrs
            [ Svg.textPath
                [ xlinkHref ("#" ++ id)
                , dominantBaseline "middle"
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


circle : String -> Coord -> String -> List (Html.Attribute msg) -> HSL -> Float -> Html msg
circle id center label externalTextAttrs hsl radius =
    let
        color =
            hslToString hsl.hue hsl.saturation hsl.lightness

        textAttrs =
            externalTextAttrs
                ++ [ x "50%"
                   , y "50%"
                   , textAnchor "middle"
                   , dominantBaseline "middle"
                   ]
    in
    g []
        [ -- The circle
          Svg.circle
            [ fill color
            , stroke "none"
            , cx (String.fromFloat center.x)
            , cy (String.fromFloat center.y)
            , r (String.fromFloat radius)
            ]
            []
        , Svg.text_ textAttrs [ Svg.text label ]
        ]
