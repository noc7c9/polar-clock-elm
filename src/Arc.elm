module Arc exposing (Coord, arc)

import Html exposing (..)
import Svg
import Svg.Attributes exposing (..)


type alias Settings =
    { center : Coord
    , innerRadius : Float
    , outerRadius : Float
    , startAngle : Float
    , endAngle : Float
    , borderRadius : Float
    }


type alias Coord =
    { x : Float, y : Float }


arc : List (Svg.Attribute msg) -> Settings -> Svg.Svg msg
arc attributes settings =
    Svg.path (attributes ++ [ d (arcPath settings) ]) []


arcPath : Settings -> String
arcPath settings =
    let
        center =
            settings.center

        innerRadius =
            settings.innerRadius

        outerRadius =
            settings.outerRadius

        borderRadius =
            -- Clamp to half the stroke width so the arc ends are half-circles
            Basics.min settings.borderRadius ((outerRadius - innerRadius) / 2)

        outerBorderRadiusAngle =
            borderRadius / outerRadius

        innerBorderRadiusAngle =
            borderRadius / innerRadius

        innerStart =
            polarToCartesian center
                innerRadius
                (settings.startAngle + innerBorderRadiusAngle)

        innerCenterStart =
            polarToCartesian center (innerRadius + borderRadius) settings.startAngle

        innerEnd =
            polarToCartesian center
                innerRadius
                (settings.endAngle - innerBorderRadiusAngle)

        innerCenterEnd =
            polarToCartesian center (innerRadius + borderRadius) settings.endAngle

        outerStart =
            polarToCartesian center
                outerRadius
                (settings.startAngle + outerBorderRadiusAngle)

        outerCenterStart =
            polarToCartesian center (outerRadius - borderRadius) settings.startAngle

        outerEnd =
            polarToCartesian center
                outerRadius
                (settings.endAngle - outerBorderRadiusAngle)

        outerCenterEnd =
            polarToCartesian center (outerRadius - borderRadius) settings.endAngle

        largeArc =
            (settings.endAngle - settings.startAngle) > pi
    in
    String.join " "
        [ moveTo innerCenterStart
        , arcTo borderRadius borderRadius innerStart False False
        , arcTo innerRadius innerRadius innerEnd largeArc True
        , arcTo borderRadius borderRadius innerCenterEnd False False
        , lineTo outerCenterEnd
        , arcTo borderRadius borderRadius outerEnd False False
        , arcTo outerRadius outerRadius outerStart largeArc False
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
