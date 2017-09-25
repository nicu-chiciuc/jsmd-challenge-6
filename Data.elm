module Data exposing (..)


type alias Point =
    ( Float, Float )


type alias Circle =
    { center : Point
    , speed : Point
    , rad : Float
    }


type alias CircleIntersection =
    Maybe ( Point, Point )
