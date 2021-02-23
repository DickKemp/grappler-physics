module Main exposing (main)

-- visualize the physics of the grappler
--
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

import Browser
import Html exposing (Html, div, input)
import Svg exposing (..)
import Svg.Attributes exposing (fill, height, points, rx, ry, stroke, transform, viewBox, width, x, y)
import Html.Attributes as Attrs exposing (type_, value)
import Html.Events exposing (onInput)
import Maybe

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
    = Angle String


type alias Point =
    ( Float, Float )


type alias Path =
    List Point


type alias BarGeometry =
    { shape : Path
    , centerOfGravity : Point
    , handlePoint : Point
    }

shiftPoint : Point -> Point -> (Float -> Float -> Float) -> Point
shiftPoint point delta op =
    let
        x = Tuple.first point
        y = Tuple.second point
        dx = Tuple.first delta
        dy = Tuple.second delta
    in
        Tuple.pair  (op x dx) (op y dy)

shiftPointDown : Point -> Point -> Point
shiftPointDown point delta =
    shiftPoint point delta (-)

shiftPointUp : Point -> Point -> Point
shiftPointUp point delta =
    shiftPoint point delta (+)

rotatePoint : Float -> Point -> Point -> Point
rotatePoint angle origin pt =
    let
        polarPoint : Point
        polarPoint = toPolar (shiftPointDown pt origin)

        rotatedPoint = ((Tuple.first polarPoint), ((Tuple.second polarPoint) + (degrees angle)))
    in
        shiftPointUp (fromPolar rotatedPoint) origin

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model barLength barWeight 0.0 0.0 (barLength - (barLength / 8)), Cmd.none )


calculateBarGeometry : Model -> BarGeometry
calculateBarGeometry model =
    let
        fx =
            Tuple.first fulcrum

        fy =
            Tuple.second fulcrum

        barGeo =  
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
    in
        
        rotateBar model.barAngle fulcrum barGeo


rotateBar : Float -> Point -> BarGeometry -> BarGeometry
rotateBar angle origin barGeo =
    BarGeometry
        (List.map (rotatePoint angle origin) barGeo.shape)
        (rotatePoint angle origin barGeo.centerOfGravity)
        (rotatePoint angle origin barGeo.handlePoint)

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
    case msg of
    Angle angle ->
        ( { model | barAngle =  Maybe.withDefault 10.0 (String.toFloat angle) } , Cmd.none )
    -- _ ->
    --    ( model, Cmd.none )



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
        ,
         input
            [ type_ "range"
            , Attrs.min "0"
            , Attrs.max "90"
            , value <| String.fromFloat model.barAngle
            , onInput Angle
            ]
            []
        , text <| String.fromFloat model.barAngle
        ]
