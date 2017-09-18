module Draw exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)

import Data exposing (..)

circleToSvg : Circle -> Svg msg
circleToSvg { center, rad } =
    circle
        [ cx <| toString <| first <| center
        , cy <| toString <| second <| center
        , r <| toString <| rad
        , fillOpacity "0.2"
        , fill "red"
        ]
        []


customPointSvg : List (Attribute msg) -> Point -> Svg msg
customPointSvg styles point =
    circle
        ([ cx <| toString <| first <| point
         , cy <| toString <| second <| point
         , r "2"
         ]
            ++ styles
        )
        []