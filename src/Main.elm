module Main exposing (main)

-- visualize the physics of the grappler
--
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

import Browser
import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (fill, height, points, rx, ry, stroke, transform, viewBox, width, x, y)


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


viewHeight : Float
viewHeight =
    600.0


viewWidth : Float
viewWidth =
    600.0

xPad =
    20.0

yPad =
    20.0
barLength : Float
barLength =
    180.0


barWidth : Float
barWidth =
    2


barWeight : Float
barWeight =
    45.0


fulcrum : Point
fulcrum =
    ( 5, 5 )


type alias Model =
    { barLength : Float
    , barWeight : Float
    , extraWeight : Float
    , barAngle : Float
    , extraWeightDistanceFromFulcrum : Float
    }


type Msg
    = BarHeight Float


type alias Point =
    ( Float, Float )


type alias Path =
    List Point


type alias BarGeometry =
    { shape : Path
    , centerOfGravity : Point
    , handlePoint : Point
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model barLength barWeight 0.0 45.0 (barLength - (barLength / 8)), Cmd.none )


calculateBarGeometry : Model -> BarGeometry
calculateBarGeometry model =
    let
        fx =
            Tuple.first fulcrum

        fy =
            Tuple.second fulcrum
    in
    BarGeometry
        [ ( fx, fy )
        , ( fx, fy + barWidth )
        , ( fx + barLength, fy + barWidth )
        , ( fx + barLength, fy )
        , ( fx + barLength, fy - barWidth )
        , ( fx, fy - barWidth )
        ]
        ( fx + (barLength / 2), fy )
        ( fx + barLength, fy )


mapPointToSvgCoordinates : Point -> Point
mapPointToSvgCoordinates point =
    let
        x = Tuple.first point
        y = Tuple.second point
    in
        (x + xPad, viewHeight - y - yPad)

mapToSvgCoordinates : BarGeometry -> BarGeometry
mapToSvgCoordinates barGeo =
    BarGeometry 
        (List.map mapPointToSvgCoordinates barGeo.shape)
        (mapPointToSvgCoordinates barGeo.centerOfGravity)
        (mapPointToSvgCoordinates barGeo.handlePoint)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none




tuple2str : Point -> String
tuple2str tpl =
    String.fromFloat (Tuple.first tpl) ++ "," ++ String.fromFloat (Tuple.second tpl)


path2svgPath : Path -> String
path2svgPath path =
    String.join " " (List.map tuple2str path)

-- VIEW

view : Model -> Html Msg
view model =
    let
        barGeo =
            mapToSvgCoordinates (calculateBarGeometry model)

        svgpath =
            path2svgPath barGeo.shape
    in
    div
        []
        [ svg
            [ width "800"
            , height "800"
            , viewBox "0 0 viewWidth viewHeight"
            ]
            [ polygon [ fill "None", stroke "black", points svgpath ] [] ]
        ]
