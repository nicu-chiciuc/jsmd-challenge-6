module Main exposing (main)

import Html exposing (Html, text, program)
import Svg exposing (..)


-- import Svg.Path exposing ()

import Svg.Attributes exposing (..)
import Tuple exposing (..)
import Data exposing (..)
import Draw exposing (..)
import Geometry exposing (..)
import AnimationFrame exposing (..)
import Time exposing (Time)


someCircles : List Circle
someCircles =
    [ { center = ( 100, 100 ), rad = 44, speed = ( -2, -2 ) } 
    , { center = ( 100, 70 ), rad = 27, speed = ( 1, -1 ) }
    , { center = ( 50, 60 ), rad = 50, speed = ( 1, 1 ) }
    , { center = ( 100, 100 ), rad = 34, speed = ( -1, 1 ) }
    , { center = ( 120, 70 ), rad = 20, speed = ( -0.56, -1 ) }
    , { center = ( 50, 60 ), rad = 50, speed = ( 0.4, -2 ) }
    , { center = ( 60, 140 ), rad = 44, speed = ( -1, 1.5 ) }
    ]


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


type alias Model =
    { circles : List Circle
    , tmpCircles : List Circle
    , svgHeight : Float
    , svgWidth : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { circles = someCircles
      , tmpCircles = []
      , svgHeight = 300
      , svgWidth = 300
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Something Time


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diffs Something
        ]


circleUpdate : Float -> Float -> Float -> Circle -> Circle
circleUpdate sWidth sHeight ms cir =
    let
        ( cx, cy ) =
            cir.center

        ( sx, sy ) =
            cir.speed

        nx =
            cx + ((sx *5 ) / ms)

        ny =
            cy + ((sy *5 ) / ms)

        nnx =
            if nx < 0 then
                nx + sWidth
            else if nx > sWidth then
                nx - sWidth
            else
                nx
        
        nny =
            if ny < 0 then
                ny + sHeight
            else if ny > sHeight then
                ny - sHeight
            else
                ny
    in
        { cir | center = ( nnx, nny ) }


getTmpCircles : Model -> List Circle
getTmpCircles model =
    let
        { circles, svgHeight, svgWidth } =
            model

        isAtMargin : Circle -> List Circle
        isAtMargin circ =
            let
                ( cx, cy ) =
                    circ.center

                rad =
                    circ.rad

                nx =
                    if (cx - rad) < 0 then
                        cx + svgWidth
                    else if (svgWidth - cx - rad) < 0 then
                        cx - svgWidth
                    else
                        cx

                ny =
                    if (cy - rad) < 0 then
                        cy + svgHeight
                    else if (svgHeight - cy - rad) < 0 then
                        cy - svgHeight
                    else
                        cy

                theOther : List (Maybe Circle)
                theOther = [
                    if (nx /= cx) then Just {circ | center = (nx, cy)} else Nothing,
                    if (ny /= cy) then Just {circ | center = (cx, ny)} else Nothing,
                    if (nx /= cx && ny /= cy) then Just {circ | center = (nx, ny)} else Nothing
                ]
            in
                List.filterMap identity theOther

        listOfCircles : List (List Circle)
        listOfCircles = List.map isAtMargin circles
        
    in
        List.concat listOfCircles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Something num ->
            let
                newc =
                    List.map (circleUpdate model.svgWidth model.svgHeight num) model.circles

                tmp =
                    getTmpCircles { model | circles = newc }
            in
                ( { model | circles = newc, tmpCircles = tmp }, Cmd.none )


view : Model -> Html msg
view model =
    let
        centers =
            List.map .center model.circles

        allCircles = model.circles ++ model.tmpCircles
    in
        svg
            [ version "1.1"
            , x "0"
            , y "0"
            , height <| toString <| model.svgHeight
            , width <| toString <| model.svgWidth
            , viewBox <| "0 0 " ++ (toString model.svgWidth) ++ " " ++ (toString model.svgHeight)
            ]
            [ g [] (List.map (circleToSvg [ fill "black" ] 0) allCircles)
            -- , g [] (List.map (circleToSvg [ fill "red" ] -6) allCircles)
            -- , g [] (List.map (circleToSvg [ fill "green" ] -12) allCircles)
            -- , g [] (List.map (circleToSvg [ fill "blue" ] -18) allCircles)
            -- , g [] (List.map (circleToSvg [ fill "yellow" ] -24) allCircles)
            , g [] (List.map (circleToSvg [ fill "white" ] -10) allCircles)
            
            ]
