module Main exposing (main)

import Html exposing (Html, text)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)
import Data exposing (..)
import Draw exposing (..)


someCircles : List Circle
someCircles =
    [ { center = ( 100, 100 ), rad = 34 }
    , { center = ( 120, 70 ), rad = 15 }
    , { center = ( 50, 60 ), rad = 50 }
    , { center = ( 60, 140 ), rad = 44 }
    ]


pointDistance : Point -> Point -> Float
pointDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


intersectingCircles : List Circle -> List ( Circle, Circle )
intersectingCircles circles =
    case circles of
        x :: xs ->
            intersectingCircleToCircles x xs ++ intersectingCircles xs

        _ ->
            []


intersectingCircleToCircles : Circle -> List Circle -> List ( Circle, Circle )
intersectingCircleToCircles circ list =
    List.filterMap (intersectingCircleCircle circ) list


intersectingCircleCircle : Circle -> Circle -> Maybe ( Circle, Circle )
intersectingCircleCircle c1 c2 =
    if pointDistance c1.center c2.center <= c1.rad + c2.rad then
        Just ( c1, c2 )
    else
        Nothing



--circleIntersection : Circle -> Circle -> CircleIntersection
--circleIntersection c1 c2 =


doline : List (Attribute msg) -> ( Circle, Circle ) -> Svg msg
doline styles ( c1, c2 ) =
    let
        ( c1x, c1y ) =
            c1.center

        ( c2x, c2y ) =
            c2.center
    in
        Svg.line
            ([ x1 <| toString <| c1x
             , y1 <| toString <| c1y
             , x2 <| toString <| c2x
             , y2 <| toString <| c2y
             , strokeWidth "2"
             , stroke "black"
             ]
                ++ styles
            )
            []


main : Html msg
main =
    let
        centers =
            List.map .center someCircles

        inters : List ( Circle, Circle )
        inters =
            intersectingCircles someCircles |> Debug.log "fuck"
    in
        svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 323.141 322.95"
            ]
            [ g [] (List.map circleToSvg someCircles)
            , g [] (List.map (customPointSvg [ fill "blue" ]) centers)
            , g [] (List.map (doline []) inters)
            ]
