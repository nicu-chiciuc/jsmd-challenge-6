module Geometry exposing (..)

import Data exposing (..)


pointDistance : Point -> Point -> Float
pointDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


mult : Point -> Float -> Point
mult ( x, y ) s =
    ( x * s, y * s )


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


sub : Point -> Point -> Point
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )



-- stackoverflow.com/questions/3349125/circle-circle-intersection-points


circleIntersection : Circle -> Circle -> CircleIntersection
circleIntersection c0 c1 =
    let
        p0 =
            c0.center

        p1 =
            c1.center

        d =
            pointDistance p0 p1

        a =
            (c0.rad ^ 2 - c1.rad ^ 2 + d ^ 2) / (2 * d)

        h =
            sqrt (c0.rad ^ 2 - a ^ 2)

        p2 =
            add p0 (mult (sub p1 p0) (a / d))

        ( x0, y0 ) =
            p0

        ( x1, y1 ) =
            p1

        ( x2, y2 ) =
            p2

        x3_1 =
            x2 + h * (y1 - y0) / d

        y3_1 =
            y2 - h * (x1 - x0) / d

        x3_2 =
            x2 - h * (y1 - y0) / d

        y3_2 =
            y2 + h * (x1 - x0) / d
    in
        Two ( ( x3_1, y3_1 ), ( x3_2, y3_2 ) )
