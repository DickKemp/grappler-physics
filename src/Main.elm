module Main exposing (main)

-- IMPORTS

import BoundedNumericValue exposing (..)
import Browser
import Element as El
    exposing
        ( Color
        , Element
        , alignBottom
        , alignRight
        , alignTop
        , column
        , el
        , fillPortion
        , padding
        , rgb255
        , rgba255
        , row
        , spacing
        )
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html, div)
import Html.Events exposing (..)
import Round exposing (..)
import Svg exposing (..)
import Svg.Attributes
    exposing
        ( d
        , fontFamily
        , fontSize
        , id
        , markerEnd
        , markerHeight
        , markerUnits
        , markerWidth
        , orient
        , points
        , refX
        , refY
        , stroke
        , viewBox
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import Orig_Main exposing (fulcrum)
-- import Orig_Main exposing (barLength)

-- TYPES

type alias BarGeometry =
    { shape : Path
    , weightShape : Path

    , centerOfGravityPoint : Point
    , centerOfGravityForceAmt : Float
    , centerOfGravityForcePoint : Point

    , handlePoint : Point
    , handleGravityForceAmt : Float
    , handleGravityForcePoint : Point

    , handleNormalForceAmt : Float
    , handleNormalForcePoint : Point

    , centerOfGravityNormalForcePoint : Point
    , centerOfGravityNormalForceAmt : Float

    , fulcrumPoint : Point
    , fulcrumGravityForceAmt : Float
    , fulcrumGravityForcePoint : Point

    }


makeEmptyBarGeometry : BarGeometry
makeEmptyBarGeometry =
    { shape = []
    , weightShape = []
    , centerOfGravityPoint = ( 0, 0 )
    , handlePoint = ( 0, 0 )
    , centerOfGravityForcePoint = ( 0, 0 )
    , centerOfGravityNormalForcePoint = ( 0, 0 )
    , handleGravityForcePoint = ( 0, 0 )
    , handleNormalForcePoint = ( 0, 0 )
    , centerOfGravityForceAmt = 0.0
    , handleGravityForceAmt = 0.0
    , centerOfGravityNormalForceAmt = 0.0
    , handleNormalForceAmt = 0.0
    , fulcrumPoint = (0,0)
    , fulcrumGravityForceAmt = 0.0
    , fulcrumGravityForcePoint = (0,0)
    }


type alias Point =
    ( Float, Float )


type alias Path =
    List Point


shiftPoint : Point -> Point -> (Float -> Float -> Float) -> Point
shiftPoint point delta op =
    ( op (getX point) (getX delta), op (getY point) (getY delta) )


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
        polarPoint =
            toPolar (shiftPointDown pt origin)

        rotatedPoint =
            ( getX polarPoint, getY polarPoint + degrees angle )
    in
    shiftPointUp (fromPolar rotatedPoint) origin


getX : ( a, b ) -> a
getX point =
    Tuple.first point


getY : ( a, b ) -> b
getY point =
    Tuple.second point

stringToFloat : String -> Float
stringToFloat strval =
    case String.toFloat strval of
        Just val ->
            val

        Nothing ->
            0.0


tuple2str : Point -> String
tuple2str tpl =
    String.fromFloat (getX tpl) ++ "," ++ String.fromFloat (getY tpl)


path2svgPath : Path -> String
path2svgPath path =
    String.join " " (List.map tuple2str path)


derivePathOfBar : Point -> Float -> Float -> Float -> Float -> Float -> ( Path, Path )
derivePathOfBar fulcrum barLength barWidth extraWeightDistanceFromFulcrum extraWeight offset =
    ( derivePathOfBarShaft fulcrum barLength barWidth offset
    , derivePathOfExtraWeight fulcrum barWidth extraWeightDistanceFromFulcrum extraWeight
    )


derivePathOfBarShaft : Point -> Float -> Float -> Float -> Path
derivePathOfBarShaft origin_ barLength_ barWidth_ offset_ =
    let
        originX =
            getX origin_

        originY =
            getY origin_

        nudge =
            5

        collarLen =
            offset_ + nudge

        innerBarLen =
            barLength_ - (2 * collarLen) - (2 * nudge)
    in
    [ ( originX, originY )
    , ( originX, originY + barWidth_ )
    , ( originX + collarLen, originY + barWidth_ )
    , ( originX + collarLen, originY + barWidth_ + nudge )
    , ( originX + collarLen + nudge, originY + barWidth_ + nudge )
    , ( originX + collarLen + nudge, originY + barWidth_ - nudge )
    , ( originX + collarLen + nudge + innerBarLen, originY + barWidth_ - nudge )
    , ( originX + collarLen + nudge + innerBarLen, originY + barWidth_ + nudge )
    , ( originX + collarLen + nudge + innerBarLen + nudge, originY + barWidth_ + nudge )
    , ( originX + collarLen + nudge + innerBarLen + nudge, originY + barWidth_ )
    , ( originX + collarLen + nudge + innerBarLen + nudge + collarLen, originY + barWidth_ ) -- top right end of bar
    , ( originX + collarLen + nudge + innerBarLen + nudge + collarLen, originY - barWidth_ ) -- bottom right end of bar
    , ( originX + collarLen + nudge + innerBarLen + nudge, originY - barWidth_ )
    , ( originX + collarLen + nudge + innerBarLen + nudge, originY - barWidth_ - nudge )
    , ( originX + collarLen + nudge + innerBarLen, originY - barWidth_ - nudge )
    , ( originX + collarLen + nudge + innerBarLen, originY - barWidth_ + nudge )
    , ( originX + collarLen + nudge, originY - barWidth_ + nudge )
    , ( originX + collarLen + nudge, originY - barWidth_ - nudge )
    , ( originX + collarLen, originY - barWidth_ - nudge )
    , ( originX + collarLen, originY - barWidth_ )
    , ( originX, originY - barWidth_ )
    ]


derivePathOfExtraWeight : Point -> Float -> Float -> Float -> Path
derivePathOfExtraWeight origin_ barWidth_ extraWeightDistanceFromFulcrum_ extraWeight_ =
    let
        originX =
            getX origin_

        originY =
            getY origin_

        nudge =
            5

        ex =
            extraWeightDistanceFromFulcrum_
    in
    [ ( originX + ex, originY )
    , ( originX + ex, originY + barWidth_ + extraWeight_ )
    , ( originX + ex + nudge, originY + (barWidth_ + extraWeight_) )
    , ( originX + ex + nudge, originY - (barWidth_ + extraWeight_) )
    , ( originX + ex, originY - (barWidth_ + extraWeight_) )
    , ( originX + ex, originY )
    ]


calculateBarGeometry : Model -> BarGeometry
calculateBarGeometry model =
    let
        originX =
            getX model.fulcrum

        originY =
            getY model.fulcrum

        offset =
            model.barLength / 8

        ( shape, weightShape ) =
            derivePathOfBar model.fulcrum model.barLength model.barWidth model.extraWeightDistanceFromFulcrum model.extraWeight offset

        centerOfGravityForceAmt =
            model.barWeight + model.extraWeight

        midX =
            model.barLength / 2

        endPointX =
            model.barLength

        extraWeightX =
            model.extraWeightDistanceFromFulcrum

        cogX =
            ((midX * model.barWeight) + (extraWeightX * model.extraWeight)) / (model.barWeight + model.extraWeight)

        d1 = cogX
        d2 = model.barLength - d1


        fulcrumPoint = model.fulcrum
        fulcrumGravityForceAmt = centerOfGravityForceAmt * (1 - ((d1/(d1+d2)) * (cos (degrees model.barAngle))^2))
        
        fulcrumGravityForcePoint = 
            ( getX fulcrumPoint
            , getY fulcrumPoint - (fulcrumGravityForceAmt * model.forceToLengthFactor)
            )

        centerOfGravityPoint =
            ( originX + cogX, originY )

        handleGravityForceAmt =
            --    centerOfGravityForce * (cogX / endPointX)
            centerOfGravityForceAmt - fulcrumGravityForceAmt
        handleGravityForcePoint =
            ( getX handlePoint - (handleGravityForceAmt * model.forceToLengthFactor * sin (degrees model.barAngle))
            , getY handleNormalForcePoint
            )
        centerOfGravityNormalForceAmt =
            centerOfGravityForceAmt * cos (degrees model.barAngle)

        handleNormalForceAmt =
            handleGravityForceAmt * cos (degrees model.barAngle)

        centerOfGravityNormalForcePoint =
            ( getX centerOfGravityPoint
            , getY centerOfGravityPoint - (centerOfGravityNormalForceAmt * model.forceToLengthFactor)
            )

        cGravityForceAmtenterOfEndPoint =
            ( getX centerOfGravityPoint - (centerOfGravityForceAmt * model.forceToLengthFactor * sin (degrees model.barAngle))
            , getY centerOfGravityNormalForcePoint
            )

        handlePoint =
            ( originX + model.barLength, originY )

        handleNormalForcePoint =
            ( getX handlePoint
            , getY handlePoint - (handleNormalForceAmt * model.forceToLengthFactor)
            )
        barGeo : BarGeometry
        barGeo =    { shape = shape
                    , centerOfGravityPoint = centerOfGravityPoint
                    , handlePoint = handlePoint
                    , centerOfGravityForcePoint = cGravityForceAmtenterOfEndPoint
                    , centerOfGravityNormalForcePoint = centerOfGravityNormalForcePoint
                    , handleGravityForcePoint = handleGravityForcePoint
                    , handleNormalForcePoint = handleNormalForcePoint
                    , centerOfGravityForceAmt = centerOfGravityForceAmt
                    , handleGravityForceAmt = handleGravityForceAmt
                    , centerOfGravityNormalForceAmt = centerOfGravityNormalForceAmt
                    , handleNormalForceAmt = handleNormalForceAmt
                    , weightShape = weightShape
                    , fulcrumPoint = fulcrumPoint
                    , fulcrumGravityForceAmt = fulcrumGravityForceAmt
                    , fulcrumGravityForcePoint = fulcrumGravityForcePoint
                    }
    in
    rotateBar model.barAngle model.fulcrum barGeo


rotateBar : Float -> Point -> BarGeometry -> BarGeometry
rotateBar angle origin barGeo =
    { shape = (List.map (rotatePoint angle origin) barGeo.shape)
    , centerOfGravityPoint = (rotatePoint angle origin barGeo.centerOfGravityPoint)
    , handlePoint = (rotatePoint angle origin barGeo.handlePoint)
    , centerOfGravityForcePoint = (rotatePoint angle origin barGeo.centerOfGravityForcePoint)
    , centerOfGravityNormalForcePoint = (rotatePoint angle origin barGeo.centerOfGravityNormalForcePoint)
    , handleGravityForcePoint = (rotatePoint angle origin barGeo.handleGravityForcePoint)
    , handleNormalForcePoint = (rotatePoint angle origin barGeo.handleNormalForcePoint)
    , centerOfGravityForceAmt = barGeo.centerOfGravityForceAmt
    , handleGravityForceAmt = barGeo.handleGravityForceAmt
    , centerOfGravityNormalForceAmt = barGeo.centerOfGravityNormalForceAmt
    , handleNormalForceAmt = barGeo.handleNormalForceAmt
    , weightShape = (List.map (rotatePoint angle origin) barGeo.weightShape)
    , fulcrumPoint = (rotatePoint angle origin barGeo.fulcrumPoint)
    , fulcrumGravityForceAmt = barGeo.fulcrumGravityForceAmt
    --, fulcrumGravityForcePoint = (rotatePoint angle origin barGeo.fulcrumGravityForcePoint)
    , fulcrumGravityForcePoint = barGeo.fulcrumGravityForcePoint
    }

mapPointToSvgCoordinates : {a | xPad : Float, yPad : Float, viewHeight: Float } -> Point -> Point
mapPointToSvgCoordinates svgCoordinates point = 
            ( getX point + svgCoordinates.xPad, svgCoordinates.viewHeight - getY point - svgCoordinates.yPad )

mapToSvgCoordinates : {a | xPad : Float, yPad : Float, viewHeight: Float } -> BarGeometry -> BarGeometry
mapToSvgCoordinates svgCoordinates barGeo =
    let
        mapPoints = mapPointToSvgCoordinates svgCoordinates
    in
        { shape = (List.map mapPoints barGeo.shape)
        , centerOfGravityPoint = (mapPoints barGeo.centerOfGravityPoint)
        , handlePoint = (mapPoints barGeo.handlePoint)
        , centerOfGravityForcePoint = (mapPoints barGeo.centerOfGravityForcePoint)
        , centerOfGravityNormalForcePoint = (mapPoints barGeo.centerOfGravityNormalForcePoint)
        , handleGravityForcePoint = (mapPoints barGeo.handleGravityForcePoint)
        , handleNormalForcePoint = (mapPoints barGeo.handleNormalForcePoint)
        , centerOfGravityForceAmt = barGeo.centerOfGravityForceAmt
        , handleGravityForceAmt = barGeo.handleGravityForceAmt
        , centerOfGravityNormalForceAmt = barGeo.centerOfGravityNormalForceAmt
        , handleNormalForceAmt = barGeo.handleNormalForceAmt
        , weightShape = (List.map mapPoints barGeo.weightShape)
        , fulcrumPoint = (mapPoints barGeo.fulcrumPoint)
        , fulcrumGravityForceAmt = barGeo.fulcrumGravityForceAmt
        , fulcrumGravityForcePoint = (mapPoints barGeo.fulcrumGravityForcePoint)
        }

-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL

type alias Model =
    { barLength : Float
    , barWeight : Float
    , barWidth : Float
    , extraWeight : Float
    , barAngle : Float
    , extraWeightDistanceFromFulcrum : Float
    , showNormalForceVector : Bool
    , showGravityForceVector : Bool
    , viewHeight : Float
    , viewWidth : Float
    , xPad : Float
    , yPad : Float
    , fulcrum : Point
    , forceToLengthFactor : Float
    , barGeo : BarGeometry
    }


type Msg
    = Angle Float
    | AngleStr String
    | NormalForceOptionChanged Bool
    | GravityForceOptionChanged Bool
    | ForceToLenFactorChanged String
    | BarLengthChanged String
    | BarWeightChanged String
    | ExtraWeightChanged String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        barLength_ =
            180 * 2.5

        model =
            { barLength = barLength_
            , barWeight = 45
            , extraWeight = 0.0
            , barAngle = 0.0
            , extraWeightDistanceFromFulcrum = barLength_ - (barLength_ / 8)
            , showNormalForceVector = False
            , showGravityForceVector = False
            , viewHeight = 600
            , viewWidth = 600
            , xPad = 20
            , yPad = 20
            , barWidth = 2 * 2.5
            , fulcrum = ( 50, 100 )
            , forceToLengthFactor = 2.0
            , barGeo = makeEmptyBarGeometry
            }
    in
    ( { model | barGeo = calculateBarGeometry model }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        newModel = 
            case msg of
                Angle angle ->
                    { model | barAngle = Maybe.withDefault 10.0 (Just angle) }
                AngleStr angle ->
                    { model | barAngle = Maybe.withDefault 10.0 (String.toFloat angle) }
                BarWeightChanged weight ->
                    { model | barWeight = Maybe.withDefault 45.0 (String.toFloat weight) }
                ExtraWeightChanged extraWeight_ ->
                    { model | extraWeight = Maybe.withDefault 0.0 (String.toFloat extraWeight_) }
                NormalForceOptionChanged b ->
                    { model | showNormalForceVector = b }
                GravityForceOptionChanged b ->
                    { model | showGravityForceVector = b }
                BarLengthChanged lengthStr ->
                    { model | barLength = stringToFloat lengthStr }
                ForceToLenFactorChanged factorStr ->
                    { model | forceToLengthFactor = stringToFloat factorStr }
    in
        ( { newModel | barGeo = calculateBarGeometry newModel }, Cmd.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    El.layout
        [ El.width El.fill, El.height El.fill, padding 5, spacing 5 ]
        (column [ bCol 1, El.width El.fill, El.height El.fill, padding 5, spacing 5 ]
            [ row [ bCol 2, El.width El.fill, El.height (fillPortion 10), padding 5, spacing 5 ]
                [ topInputsPane model, rightInputsPane model ]
            , row [ bCol 9, alignBottom, El.width El.fill, El.height (fillPortion 1), padding 5, spacing 5 ]
                [ el [] (El.text (("angle: " ++ Round.round 2 model.barAngle) ++ " degrees")) ]
            ]
        )


topInputsPane : Model -> Element Msg
topInputsPane model =
    column [ bCol 3, El.width (fillPortion 9), alignTop, padding 5, spacing 5, El.height El.fill ]
        [ topInputs model
        , contentsPane model 
        ]


rightInputsPane : Model -> Element Msg
rightInputsPane model =
    let
        slider =
            FloatValue { min = 0, max = 90, message = Angle, label = "" }
    in
    column [ bCol 4, El.width (fillPortion 1), padding 5, spacing 5, El.height El.fill ]
        [ drawInput slider model.barAngle Vertical ]


topInputs : Model -> Element Msg
topInputs model =
    row [ bCol 5, alignTop, padding 5, spacing 25, El.width El.fill ]
        [ inputPane model, checkboxesPane model ]


contentsPane : Model -> Element Msg
contentsPane model =
    row [ bCol 6, El.height El.fill, El.width El.fill, alignBottom, padding 5, spacing 5 ]
        [ el [ El.height El.fill, El.width El.fill, padding 5, spacing 5 ]
            (El.html 
                (   
                    let
                        svgCoordinates = {xPad=model.xPad, yPad=model.yPad, viewHeight=model.viewHeight}
                        barGeo = (mapToSvgCoordinates svgCoordinates model.barGeo)
                    in
                    svg [ Svg.Attributes.width "600"
                        , Svg.Attributes.height "600"
                        , viewBox ("0 0 " ++ String.fromFloat model.viewWidth ++ " " ++ String.fromFloat model.viewHeight)
                        ]
                        [ drawBar barGeo.shape barGeo.weightShape
                        , if model.showGravityForceVector == True then
                            drawForceVector
                                barGeo.centerOfGravityPoint
                                barGeo.centerOfGravityForcePoint
                                "blue"
                                (Round.round 2 barGeo.centerOfGravityForceAmt ++ " lbs")
                                False

                        else
                            div [] []
                        , if model.showGravityForceVector == True then
                            drawForceVector
                                (shiftY barGeo.fulcrumPoint model.barWidth)
                                barGeo.fulcrumGravityForcePoint
                                "red"
                                (Round.round 2 barGeo.fulcrumGravityForceAmt ++ " lbs")
                                True

                        else
                            div [] []
                        , if model.showGravityForceVector == True && model.showNormalForceVector == False then
                            drawForceVector
                                (shiftY barGeo.handlePoint model.barWidth)
                                barGeo.handleGravityForcePoint
                                "red"
                                (Round.round 2 barGeo.handleGravityForceAmt ++ " lbs")
                                True

                        else
                            div [] []
                        , if model.showNormalForceVector == True then
                            drawForceVector
                                (shiftY barGeo.handlePoint model.barWidth)
                                barGeo.handleNormalForcePoint
                                "green" 
                                (Round.round 2 barGeo.handleNormalForceAmt ++ " lbs")
                                True

                        else
                            div [] []
                    ]
                )
            )
        ]

shiftY : Point -> Float -> Point 
shiftY (x,y) n = (x, y + n)

inputPane : Model -> Element Msg
inputPane model =
    column
        [ bCol 7, padding 5, spacing 5, El.width (El.fill |> El.maximum 200 |> El.minimum 100) ]
        [ column [ bCol 21, El.width El.fill, padding 5, spacing 5, El.height El.fill ]
            [ Input.text []
                { onChange = BarWeightChanged
                , text = String.fromFloat model.barWeight
                , placeholder = Just <| Input.placeholder [] <| El.text "Type here"
                , label = Input.labelLeft [] <| El.text "Bar Weight"
                }
            , Input.text []
                { onChange = ExtraWeightChanged
                , text = String.fromFloat model.extraWeight
                , placeholder = Just <| Input.placeholder [] <| El.text "Type here"
                , label = Input.labelLeft [] <| El.text "Extra Weight"
                }
            ]
        ]


checkboxesPane : Model -> Element Msg
checkboxesPane model =
    column [ bCol 8, padding 5, spacing 5, alignRight ]
        [ drawCheckbox "show normal force vector" NormalForceOptionChanged model.showNormalForceVector
        , drawCheckbox "show gravity force vector" GravityForceOptionChanged model.showGravityForceVector
        ]


drawBar : Path -> Path -> Svg msg
drawBar shape weight =
    g []
        [ polygon [ Svg.Attributes.fill "None", stroke "black", points (path2svgPath shape) ] []
        , polygon [ Svg.Attributes.fill "black", stroke "black", points (path2svgPath weight) ] []
        ]


drawForceVector : Point -> Point -> String -> String -> Bool -> Svg msg
drawForceVector start_ end_ color_ label_ flip =
    let
        start = if flip == True then end_ else start_
        end = if flip == True then start_ else end_

        label_end = if flip == True then start else end
    in
        g []
        -- here we define the arrowhead that we will use when drawing force vectors
        [ defs [] 
            [ marker
                [ id "arrowhead"
                , viewBox "0 0 10 10"
                , markerUnits "strokeWidth"
                , markerWidth "10"
                , markerHeight "10"
                , refX "10"
                , refY "5"
                , orient "auto"
                ]
                [ path [ d "M 0 0 L 10 5 L 0 10 z", Svg.Attributes.fill "black" ] [] ]
            ]
            , line
            [ x1 (String.fromFloat (getX start))
            , y1 (String.fromFloat (getY start))
            , x2 (String.fromFloat (getX end))
            , y2 (String.fromFloat (getY end))
            , stroke color_
            , markerEnd "url(#arrowhead)"
            ]
            []
        , text_
            [ fontFamily "sans-serif"
            , fontSize "14, x 5, y 65"
            , x (String.fromFloat (getX label_end))
            , y (String.fromFloat (getY label_end + 20))
            , Svg.Attributes.fill color_
            ]
            [ Html.text label_ ]
        ]


color1 : { col1 : Color, col2 : Color, col3 : Color, col4 : Color, col5 : Color, col6 : Color, col7 : Color, col8 : Color, col9 : Color, col10 : Color, col11 : Color, col10a : Color, col11a : Color }
color1 =
    { col1 = rgb255 10 10 10
    , col2 = rgb255 50 50 50
    , col3 = rgb255 100 100 100
    , col4 = rgb255 50 100 200
    , col5 = rgb255 200 100 50
    , col6 = rgb255 50 200 200
    , col7 = rgb255 90 200 10
    , col8 = rgb255 123 166 183
    , col9 = rgb255 123 166 183
    , col10 = rgb255 255 255 255
    , col11 = rgb255 0 0 0
    , col10a = rgba255 255 255 255 0
    , col11a = rgba255 0 0 0 0
    }


bCol : Int -> El.Attr decorative msg
bCol i =
    -- let
    --     r = 2 * i * 5
    --     g = 4 * i * 5
    --     b = 3 * i * 5
    -- in
    -- Background.color (rgb255 r g b)
    if i == 1 then
        Background.color color1.col11

    else
        Background.color color1.col10
