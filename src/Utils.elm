module Utils exposing (Coord, hslToString, polarToCartesian)


type alias Coord =
    { x : Float, y : Float }


polarToCartesian : Coord -> Float -> Float -> Coord
polarToCartesian center radius angle =
    { x = center.x + radius * sin angle
    , y = center.y - radius * cos angle
    }


hslToString : Float -> Float -> Float -> String
hslToString h s l =
    let
        toPercent value =
            let
                clamped =
                    max 0.0 (min 1.0 value)
            in
            String.fromFloat (clamped * 100) ++ "%"

        toDegrees radians =
            radians * 180 / pi
    in
    "hsl(" ++ String.fromFloat (toDegrees h) ++ ", " ++ toPercent s ++ ", " ++ toPercent l ++ ")"
