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
    , arcBorderRadius : Float
    , colorInitial : Float
    , colorDifference : Float
    , yearColorDifference : Float
    , saturation : Float
    , lightness : Float
    , fontFamily : String
    , fontSize : String
    , textPad : Float
    , textPosition : Float
    , hideYearCircle : Bool
    }


polarClock : Settings -> Time.Zone -> Time.Posix -> Html msg
polarClock settings zone posix =
    let
        center =
            Coord 50 50

        continuous =
            TimeUtils.toContinuousTimes zone posix

        secondPercent =
            continuous.seconds / 60

        minutePercent =
            continuous.minutes / 60

        hourPercent =
            continuous.hours / 24

        weekdayPercent =
            continuous.weekdays / 7

        monthdayPercent =
            continuous.days / toFloat (TimeUtils.toDaysInMonth zone posix)

        monthPercent =
            continuous.months / 12

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

        color index angle =
            let
                hue =
                    settings.colorInitial + (index * settings.colorDifference)
            in
            hslToString (angle + hue) settings.saturation settings.lightness

        textAttrs =
            [ fontSize settings.fontSize
            , fontFamily settings.fontFamily
            ]

        parameterisedArc index id label percent =
            let
                gapAngle =
                    (settings.gapWidth / 2) / ((inner + outer) / 2)

                inner =
                    innerRadius index

                outer =
                    outerRadius index

                start =
                    gapAngle

                end =
                    (2 * pi - 2 * gapAngle) * percent + gapAngle

                textPosition =
                    Basics.max 0.0 (Basics.min 1.0 settings.textPosition)

                textRadius =
                    (inner * textPosition) + (outer * (1 - textPosition))

                col =
                    color index (2 * pi * percent)
            in
            arc id
                center
                label
                settings.textPad
                textRadius
                textAttrs
                col
                settings.arcBorderRadius
                inner
                outer
                start
                end

        yearColorOffset =
            continuous.years * settings.yearColorDifference
    in
    svg [ viewBox "0 0 100 100" ] <|
        [ parameterisedArc 0 "seconds" secondLabel secondPercent
        , parameterisedArc 1 "minutes" minuteLabel minutePercent
        , parameterisedArc 2 "hours" hourLabel hourPercent
        , parameterisedArc 3 "day-of-week" weekdayLabel weekdayPercent
        , parameterisedArc 4 "day-of-month" monthdayLabel monthdayPercent
        , parameterisedArc 5 "month" monthLabel monthPercent
        ]
            ++ (if settings.hideYearCircle then
                    []

                else
                    [ circle "year" center yearLabel textAttrs (color 6 yearColorOffset) (outerRadius 6) ]
               )


arc : String -> Coord -> String -> Float -> Float -> List (Html.Attribute msg) -> String -> Float -> Float -> Float -> Float -> Float -> Html msg
arc id center label textPad textRadius textAttrs color borderRadius innerRadius outerRadius startAngle endAngle =
    let
        centerRadius =
            (innerRadius + outerRadius) / 2

        textFlipped =
            endAngle < (pi / 2) || endAngle > (3 * pi / 2)

        textPadRadius =
            textPad / centerRadius

        arcPath =
            SvgUtils.arcShapePath center innerRadius outerRadius startAngle endAngle borderRadius

        textPath =
            SvgUtils.arcLinePath center textRadius (endAngle - textPadRadius - pi) (endAngle - textPadRadius) (not textFlipped)

        textPathElement =
            Svg.textPath
                [ xlinkHref ("#" ++ id ++ "-path")
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
    in
    g []
        [ -- black circle stroke in the background
          Svg.circle
            [ fill "none"
            , stroke "black"
            , cx (String.fromFloat center.x)
            , cy (String.fromFloat center.y)
            , r (String.fromFloat centerRadius)
            ]
            []

        -- The path for the label text
        , Svg.path
            [ Html.Attributes.id (id ++ "-path")
            , fill "none"
            , stroke "none"
            , d textPath
            ]
            []

        -- The white background label text
        , Svg.text_ (fill "white" :: textAttrs) [ textPathElement ]

        -- The arc
        , Svg.path [ fill color, stroke "none", d arcPath ] []

        -- The arc as a clip-path for the label text
        , Svg.defs []
            [ Svg.clipPath
                [ Html.Attributes.id (id ++ "-clip-path") ]
                [ Svg.path [ fill color, stroke "none", d arcPath ] [] ]
            ]

        -- The black foreground label text
        , Svg.text_
            (Svg.Attributes.clipPath ("url(#" ++ id ++ "-clip-path)")
                :: textAttrs
            )
            [ textPathElement ]
        ]


circle : String -> Coord -> String -> List (Html.Attribute msg) -> String -> Float -> Html msg
circle id center label externalTextAttrs color radius =
    let
        textAttrs =
            externalTextAttrs
                ++ [ x "50%"
                   , y "50%"
                   , textAnchor "middle"
                   , dominantBaseline "central"
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
