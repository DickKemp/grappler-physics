module Main exposing (main)

-- IMPORTS

import BoundedNumericValue exposing (drawInput, BoundedNumericValue(..), Orientation(..), drawCheckbox)
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
import Svg.Attributes exposing (values)
import Svg.Attributes exposing (fontWeight)
--import Basics.round

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

    , viewHeight : Float
    , viewWidth : Float

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
    , viewHeight=0.0
    , viewWidth=0.0
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

shiftY : Point -> Float -> Point 
shiftY (x,y) n = shiftPointUp (x,y) (0,n)


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


derivePathOfBar : Point -> Float -> Float -> Float -> Float -> ( Path, Path )
derivePathOfBar fulcrum barLength barWidth collarLen extraWeight  =
    ( derivePathOfBarShaft fulcrum barLength barWidth collarLen 
    , derivePathOfExtraWeight fulcrum barWidth (barLength - collarLen) extraWeight
    )


derivePathOfBarShaft : Point -> Float -> Float -> Float -> Path
derivePathOfBarShaft origin_ barLength_ barWidth_ collarLen_ =
    let
        originX =
            getX origin_

        originY =
            getY origin_

        nudge =
            5

        collarLen =
            collarLen_ + nudge

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
        extraWeightDrawn = squash 50 0.75 extraWeight_
        ex =
            extraWeightDistanceFromFulcrum_
    in
    [ ( originX + ex, originY )
    , ( originX + ex, originY + barWidth_ + extraWeightDrawn )
    , ( originX + ex + nudge, originY + (barWidth_ + extraWeightDrawn) )
    , ( originX + ex + nudge, originY - (barWidth_ + extraWeightDrawn) )
    , ( originX + ex, originY - (barWidth_ + extraWeightDrawn) )
    , ( originX + ex, originY )
    ]


calculateBarGeometry : Model -> BarGeometry
calculateBarGeometry model =
    let
        extraWeight = Maybe.withDefault 0.0 (String.toFloat model.extraWeightStr)
        centerOfGravityForceAmt =
            model.barWeight + extraWeight
        min_origin_y = centerOfGravityForceAmt * model.forceToLengthFactor

        --fulcrumPoint = shiftY model.fulcrum min_origin_y
        fulcrumPoint = (model.distanceOfBarPivotFromLeft, model.heightOfBarPivot)

        viewHeight_ = getY fulcrumPoint + model.barLength + 20
        viewWidth_ = getX fulcrumPoint + (model.barLength * 1.5) + 5

        originX =
            getX fulcrumPoint

        originY =
            getY fulcrumPoint

        extraWeightX =
            model.barLength - model.collarLength

        ( shape, weightShape ) =
            derivePathOfBar fulcrumPoint model.barLength model.barWidth model.collarLength extraWeight
        
        midX =
            model.barLength / 2

        cogX =
            ((midX * model.barWeight) + (extraWeightX * extraWeight)) / (model.barWeight + extraWeight)

        d1 = cogX
        d2 = model.barLength - d1

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
                    , viewHeight=viewHeight_
                    , viewWidth=viewWidth_
                    }
    in
    rotateBar model.barAngle fulcrumPoint barGeo


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
    , viewHeight=barGeo.viewHeight
    , viewWidth=barGeo.viewWidth
    }

mapToSvgCoordinates : {a | viewHeight: Float, viewWidth : Float } -> BarGeometry -> BarGeometry
mapToSvgCoordinates svgCoordinates barGeo =
    let
        mapPoints point = 
            ( getX point, svgCoordinates.viewHeight - getY point )
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
        , viewHeight=barGeo.viewHeight
        , viewWidth=barGeo.viewWidth
        }

-- this type is here just to act as a bookmark in vscode
type alias A_main = ()
-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


-- this type is here just to act as a bookmark in vscode
type alias A_Model = ()
-- MODEL

type alias Model =
    { barLength : Float
    , barWeight : Float
    , barWidth : Float
    , collarLength : Float
    , barAngle : Float
    , showNormalForceVector : Bool
    , showGravityForceVector : Bool
    , heightOfBarPivot : Float
    , distanceOfBarPivotFromLeft : Float
    , forceToLengthFactor : Float
    , barGeo : BarGeometry
    , barSize : BarSize
    , extraWeightStr : String
    }

-- this type is here just to act as a bookmark in vscode
type alias A_Msg = ()
type Msg
    = Angle Float
    | AngleStr String
    | NormalForceOptionChanged Bool
    | GravityForceOptionChanged Bool
    | ForceToLenFactorChanged String
    | BarLengthChanged String
    | BarWeightChanged String
    | ExtraWeightChanged String
    | BarSizeChanged BarSize

type BarSize = 
    BigBar | LittleBar

-- this type is here just to act as a bookmark in vscode
type alias A_init = ()
-- INIT

unitsPerInchFactor : Float
unitsPerInchFactor = 450.0/86.0

init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { barLength = 86.0 * unitsPerInchFactor
            , barWeight = 45
            , barWidth = 1.33 * unitsPerInchFactor
            , barAngle = 0.0
            , collarLength = 16.25 * unitsPerInchFactor
            , heightOfBarPivot = 12.0 * unitsPerInchFactor
            , distanceOfBarPivotFromLeft = 20
            , showNormalForceVector = False
            , showGravityForceVector = True
            , forceToLengthFactor = 5.0
            , barGeo = makeEmptyBarGeometry
            , barSize = BigBar
            , extraWeightStr = "0.0"
            }
    in
    ( { model | barGeo = calculateBarGeometry model }, Cmd.none )

-- this type is here just to act as a bookmark in vscode
type alias A_update = ()
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
                    { model | extraWeightStr = extraWeight_ }
                NormalForceOptionChanged b ->
                    { model | showNormalForceVector = b }
                GravityForceOptionChanged b ->
                    { model | showGravityForceVector = b }
                BarLengthChanged lengthStr ->
                    { model | barLength = stringToFloat lengthStr }
                ForceToLenFactorChanged factorStr ->
                    { model | forceToLengthFactor = stringToFloat factorStr }
                BarSizeChanged BigBar ->
                    { model | barSize = BigBar
                    , barWeight =  45.0
                    , barLength = 86.0 * unitsPerInchFactor 
                    , collarLength = 16.25 * unitsPerInchFactor
                    }
                BarSizeChanged LittleBar ->
                    { model | barSize = LittleBar
                    , barWeight =  15.0 
                    , barLength = 64.0 * unitsPerInchFactor 
                    , collarLength = 6.0 * unitsPerInchFactor
                    }
    in
        ( { newModel | barGeo = calculateBarGeometry newModel }, Cmd.none )


-- this type is here just to act as a bookmark in vscode
type alias A_subscriptions = ()
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- this type is here just to act as a bookmark in vscode
type alias A_view = ()
-- VIEW

-- debugView : Attribute msg
debugView = 
--    El.explain Debug.todo 
    El.pointer
-- debugView = el [] (El.html (div [] []))
-- debugView = None

view : Model -> Html Msg
view model =
    El.layout
        [ El.width El.fill, El.height El.fill, padding 5, spacing 5 
         , debugView
        ]
        (column [ bCol 1, El.width El.fill
        --, El.height El.fill
        , padding 5, spacing 5 , debugView]
            [ row [ 
                bCol 2, El.width El.fill
                -- , El.height (fillPortion 10)
                , padding 5, spacing 5 , debugView]
                [ topInputsPane model
                -- , rightInputsPane model 
                ]
            -- , row [ bCol 9, alignBottom, El.width El.fill, El.height (fillPortion 1), padding 0, spacing 0 , debugView]
               , row [ bCol 9, alignBottom, El.width El.fill, padding 0, spacing 0 , debugView]
                [ el [] (El.text (("angle: " ++ Round.round 2 model.barAngle) ++ " degrees")) ]
            ]
        )

     
topInputsPane : Model -> Element Msg
topInputsPane model =
    column [ bCol 3, El.width (fillPortion 9), alignTop, padding 5, spacing 5, El.height El.fill 
            , debugView 
            ]
        [ topInputs model
        , contentsPane model 
        ]


rightInputsPane : Model -> Element Msg
rightInputsPane model =
    let
        slider =
            FloatValue { min = 0, max = 90, message = Angle, label = "" }
    in
    column [ bCol 4, El.width (fillPortion 1), padding 5, spacing 5, El.height El.fill , debugView]
        [ drawInput slider model.barAngle Vertical 300]


topInputs : Model -> Element Msg
topInputs model =
    row [ bCol 5, alignTop, padding 5, spacing 25
        -- , El.width El.fill 
        , debugView        
    ]
        [ inputPane model
        -- , checkboxesPane model 
            , column [] [
            drawCheckbox "show normal force vector" NormalForceOptionChanged model.showNormalForceVector
            , drawCheckbox "show gravity force vector" GravityForceOptionChanged model.showGravityForceVector
        ]
        ]

inputPane : Model -> Element Msg
inputPane model =
            column [ bCol 10
            -- , El.width El.fill
            , padding 5, spacing 5, El.height El.fill, debugView ]
            [Input.radio
                [ padding 5
                , spacing 5
                ]
                { onChange = BarSizeChanged
                , selected = Just model.barSize
                , label = Input.labelAbove [] <| (El.text "Bar")
                , options =
                    [ Input.option BigBar (El.text "45 lb")
                    , Input.option LittleBar (El.text "15 lb")
                    ]
                }
            , inputText "Extra Weight : " model.extraWeightStr ExtraWeightChanged
            ]

inputText : String -> String -> (String -> msg) -> Element msg
inputText txt f m = 
 Input.text [El.width (El.px 80), padding 1, spacing 1]
        { onChange = m
        , text = f
        , placeholder = Just <| Input.placeholder [] (El.text "Type here")
        , label = Input.labelLeft [] (El.text txt)
        }

checkboxesPane : Model -> Element Msg
checkboxesPane model =
    column [ bCol 8, padding 5, spacing 5, alignRight, debugView ]
        [ drawCheckbox "show normal force vector" NormalForceOptionChanged model.showNormalForceVector
        , drawCheckbox "show gravity force vector" GravityForceOptionChanged model.showGravityForceVector
        ]


contentsPane : Model -> Element Msg
contentsPane model =
    let
        slider =
            FloatValue { min = 0, max = 90, message = Angle, label = "" }
    in
    row [ bCol 6, El.height El.fill
        , alignTop
        --, alignBottom
        , padding 0, spacing 0
        , debugView
         ]
        [  
          el [ El.height El.fill, El.width El.fill, padding 0, spacing 0 ]
            (El.html 
                (   
                    let
                        svgCoordinates = {viewHeight=model.barGeo.viewHeight, viewWidth=model.barGeo.viewWidth}
                        barGeo = (mapToSvgCoordinates svgCoordinates model.barGeo)
                    in
                    svg [ Svg.Attributes.width (String.fromFloat barGeo.viewWidth)
                        , Svg.Attributes.height (String.fromFloat barGeo.viewHeight)
                        , Svg.Attributes.preserveAspectRatio "xMinYMin meet"
                        , viewBox ("0 0 " ++ String.fromFloat barGeo.viewWidth ++ " " ++ String.fromFloat barGeo.viewHeight)
                        ]
                        [ rect [  Svg.Attributes.height (String.fromFloat barGeo.viewHeight)
                                , Svg.Attributes.width (String.fromFloat barGeo.viewWidth)
                                , Svg.Attributes.fill "lightgreen"] 
                                []
                        , drawBar barGeo.shape barGeo.weightShape
                        , drawFrame barGeo
                        , if model.showGravityForceVector == True then
                            drawForceVector
                                barGeo.centerOfGravityPoint
                                barGeo.centerOfGravityForcePoint
                                "blue"
                                (Round.round 2 barGeo.centerOfGravityForceAmt ++ " lbs")
                                False
                                barGeo
                        else
                            div [] []
                        , if model.showGravityForceVector == True then
                            drawForceVector
                                (shiftY barGeo.fulcrumPoint model.barWidth)
                                barGeo.fulcrumGravityForcePoint
                                "red"
                                (Round.round 2 barGeo.fulcrumGravityForceAmt ++ " lbs")
                                True
                                barGeo

                        else
                            div [] []
                        , if model.showGravityForceVector == True then
                            drawForceVector
                                (shiftY barGeo.handlePoint model.barWidth)
                                barGeo.handleGravityForcePoint
                                "red"
                                (Round.round 2 barGeo.handleGravityForceAmt ++ " lbs")
                                True
                                barGeo

                        else
                            div [] []
                        , if model.showNormalForceVector == True then
                            drawForceVector
                                (shiftY barGeo.handlePoint model.barWidth)
                                barGeo.handleNormalForcePoint
                                "black" 
                                (Round.round 2 barGeo.handleNormalForceAmt ++ " lbs")
                                True
                                barGeo

                        else
                            div [] []
                    ]
                )
            )
        , drawInput slider model.barAngle Vertical (Basics.round model.barGeo.viewHeight)
        ]

squash: Float -> Float -> Float -> Float
squash maxVal thresholdPercent val =
    let
        threshold = maxVal * thresholdPercent
    in
        if val < threshold then val 
        else if val < maxVal then val 
        else maxVal

pointOnLine : Point -> Point -> Float -> (Float, Float)
pointOnLine p0 p1 dt =
    let
        d = lineLen p0 p1
        t = dt/d
        tx = ((1 - t) * (getX p0)) + (t * (getX p1))
        ty = ((1 - t) * (getY p0)) + (t * (getY p1))
    in
        (tx, ty)
    
sigmoid : Float -> Float
sigmoid x = (1.0 / (1.0 + e^(-x)))

tanh : Float -> Float
tanh x = (e^(2*x) - 1)/(e^(2*x) + 1)

formatDistance : Float -> String
formatDistance d = 
    let
        feet = Basics.floor (d / 12.0)
        inches = d - toFloat (12 * feet)

    in
        (String.fromInt feet) ++ " ft, " ++ (Round.round 2 inches) ++ " in"

drawFrame : BarGeometry -> Svg msg
drawFrame barGeo =
    let
        -- xc = barGeo
        leftX = 0.0
        leftY = barGeo.viewHeight
        barEndHeight = (barGeo.viewHeight - (getY barGeo.handlePoint))/unitsPerInchFactor
    in
    g []
    [
        line [ x1 (String.fromFloat leftX)
             , y1 (String.fromFloat leftY)
             , x2 (String.fromFloat (leftX + barGeo.viewWidth))
             , y2 (String.fromFloat leftY)
             , stroke "red"
            ]
            []
        , line 
            [ x1 (String.fromFloat (leftX + barGeo.viewWidth - 36.0))
            , y1 (String.fromFloat (getY barGeo.handlePoint))
            , x2 (String.fromFloat (leftX + barGeo.viewWidth - 36.0))
            , y2 (String.fromFloat leftY)
            , Svg.Attributes.strokeDasharray "14, 14"
            , stroke "red"
            ]
            []
        , line 
            [ x1 (String.fromFloat (leftX + barGeo.viewWidth - 66.0))
            , y1 (String.fromFloat (getY barGeo.handlePoint))
            , x2 (String.fromFloat (leftX + barGeo.viewWidth - 6.0))
            , y2 (String.fromFloat (getY barGeo.handlePoint))
            , stroke "red"
            ]
            []            
        , text_
            [ fontFamily "sans-serif"
            -- , fontSize "14, x 5, y 65"
            , fontSize "14"
            , x (String.fromFloat (leftX + barGeo.viewWidth - 80.0))
            , y (String.fromFloat ((getY barGeo.handlePoint) - 3))
            , Svg.Attributes.fill "black"
            ]
            [Html.text (formatDistance barEndHeight)]
    ]

drawBar : Path -> Path -> Svg msg
drawBar shape weight =
    g []
        [ polygon [ Svg.Attributes.fill "white", stroke "black", points (path2svgPath shape) ] []
        , polygon [ Svg.Attributes.fill "black", stroke "black", points (path2svgPath weight) ] []
        ]

lineLen : Point -> Point -> Float
lineLen p1 p2 =
    let
        x1 = getX p1
        y1 = getY p1
        x2  = getX p2
        y2 = getY p2
    in
        sqrt ((x2 - x1)^2 + (y2 - y1)^2)

drawForceVector : Point -> Point -> String -> String -> Bool -> BarGeometry -> Svg msg
drawForceVector start_ end_ color_ label_ reverse_arrow  barGeo =
    let
        len = lineLen start_ end_
        maxLen = barGeo.viewHeight - getY start_ - 25
        squashedLen = squash maxLen 0.75 len
        pointEnd = pointOnLine start_ end_ squashedLen

        (start, end) =
            if reverse_arrow == True 
            then
                (pointEnd, start_)
            else
                (start_, pointEnd)

        label_end = pointEnd
        -- final_label = label_ ++ " - (" ++ Round.round 2 (getY pointEnd) ++ ", " ++ Round.round 2 len ++ ", " ++ Round.round 2 squashedLen ++ ")"
        final_label = label_ 
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
            [ -- fontFamily "sans-serif"
              fontFamily "consolas"
            , fontWeight "bold"
            , fontSize "14"
            , x (String.fromFloat (getX label_end))
            , y (String.fromFloat (getY label_end + 20))
            , Svg.Attributes.fill color_
            ]
            [ Html.text final_label ]
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
