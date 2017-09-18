module Data exposing (..)

type alias Point =
    ( Float, Float )


type alias Circle =
    { center : Point
    , rad : Float
    }


type CircleIntersection
    = Two ( Point, Point )
    | One Point
    | None