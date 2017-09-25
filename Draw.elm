module Draw exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)
import Data exposing (..)


circleToSvg : List (Attribute msg) -> Float -> Circle -> Svg msg
circleToSvg styles radAdd { center, rad } =
    let 
        nrad = rad + radAdd
        nnrad = if nrad < 0 then 0 else nrad
    in
    circle
        ([ cx <| toString <| first <| center
         , cy <| toString <| second <| center
         , r <| toString <|  nnrad
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
