module Draw exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)
import Data exposing (..)


circleToSvg : List (Attribute msg) -> Float -> Circle -> Svg msg
circleToSvg styles radAdd { center, rad } =
    circle
        ([ cx <| toString <| first <| center
         , cy <| toString <| second <| center
         , r <| toString <| rad + radAdd
         ]
            ++ styles
        )
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
