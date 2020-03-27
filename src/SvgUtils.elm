module SvgUtils exposing (arcLinePath, arcShapePath)

import Utils exposing (Coord, polarToCartesian)


arcLinePath : Coord -> Float -> Float -> Float -> Bool -> String
arcLinePath center radius startAngle endAngle reverse =
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


arcShapePath : Coord -> Float -> Float -> Float -> Float -> Float -> String
arcShapePath center innerRadius outerRadius startAngle endAngle rawBorderRadius =
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
