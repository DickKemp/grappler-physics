module Main exposing (main, viewHeight)

-- visualize the physics of the grappler
--
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

import Browser
import Dict
import Html exposing (Html, br, div, fieldset, input, label)
import Html.Attributes as Attrs exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element as El exposing (Element, alignTop, alignLeft, alignBottom, alignRight, 
    centerY, centerX, column, el, fill, fillPortion, height, padding, px, rgb255, rgba255, row, spacing, text, width)

import Element.Input as Input

import Round exposing (round)
import Svg exposing (..)
import Svg.Attributes
    exposing
        ( d
        , color
        , fill
        , fontFamily
        , fontSize
        , height
        , id
        , markerEnd
        , markerHeight
        , markerUnits
        , markerWidth
        , orient
        , points
        , refX
        , refY
        , rx
        , ry
        , stroke
        , transform
        , viewBox
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )

-- import Browser.Dom exposing (Element)

import BoundedNumericValue exposing (..)
main : Program () Model (Msg Bool)
main =
    Browser.element { init = init, subscriptions=subscriptions, update = update, view = view }


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
    180.0 * 2.5


barWidth : Float
barWidth =
    2 * 2.5


barWeight : Float
barWeight =
    45.0


fulcrum : Point
fulcrum =
    ( 50, 100 )


forceToLengthFactor : Float
forceToLengthFactor =
    2.2


type alias Model =
    { barLength : Float
    , barWeight : Float
    , extraWeight : Float
    , barAngle : Float
    , extraWeightDistanceFromFulcrum : Float
    , showNormalForceVector: Bool
    , showGravityForceVector: Bool
    , viewHeight : Float
    , viewWidth : Float
    , xPad : Float
    , yPad : Float
    , barWidth : Float
    , fulcrum : Point
    , forceToLengthFactor : Float
    }


showCoGForceVector =
    "Display gravitational force vector at the bar's center of gravity"


showCogNormalForceVector =
    "Dispay normal force vector at the bar's center of gravity"


showHandleGravityForceVector =
    "Display gravitational force vector at the end of the bar"


showHandleNormalForceVector =
    "Display normal force vector at the end of the bar"


showLabels =
    "showLabels"



type Msg m
    = Angle Float
    | AngleStr String
    | NormalForceOptionChanged Bool
    | GravityForceOptionChanged Bool
    | ForceToLenFactorChanged String
    | BarLengthChanged String
    | BarWeightChanged String
    | ExtraWeightChanged String

type alias Point =
    ( Float, Float )


type alias Path =
    List Point


type alias BarGeometry =
    { shape : Path
    , centerOfGravityPoint : Point
    , handlePoint : Point
    , cogGravityForceEndPoint : Point
    , cogNormalForceEndPoint : Point
    , handleGravityForceEndPoint : Point
    , handleNormalForceEndPoint : Point
    , cogGravityForce : Float
    , endPointGravityForce : Float
    , cogNormalForce : Float
    , endPointNormalForce : Float
    , weightShape : Path
    }


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


init : () -> ( Model, Cmd (Msg m) )
init _ =

    ( Model barLength
        barWeight
        -- extra weight added to bar
        0.0
        -- bar angle in degrees
        0.0
        -- extraWeightDistanceFromFulcrum
        (barLength - (barLength / 8))
        False
        True
        viewHeight
        viewWidth
        xPad
        yPad
        barWidth
        fulcrum
        -- forceToLengthFactor
        1.0
    , Cmd.none
    )


getX point =
    Tuple.first point


getY point =
    Tuple.second point


calcBarShape2 : Point -> Float -> Float -> Float -> List Point
calcBarShape2 origin_ barLength_ barWidth_ offset_ =
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


calcWeightShape origin_ barWidth_ extraWeightDistanceFromFulcrum_ extraWeight_ =
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

        shape =
            calcBarShape2 model.fulcrum model.barLength model.barWidth offset

        weightShape =
            calcWeightShape model.fulcrum model.barWidth model.extraWeightDistanceFromFulcrum model.extraWeight

        centerOfGravityForce =
            model.barWeight + model.extraWeight

        midX =
            model.barLength / 2

        endPointX =
            model.barLength

        extraWeightX =
            model.extraWeightDistanceFromFulcrum

        cogX =
            ((midX * model.barWeight) + (extraWeightX * model.extraWeight)) / (model.barWeight + model.extraWeight)

        centerOfGravityPoint =
            ( originX + cogX, originY )

        endPointGravityForce =
            centerOfGravityForce * (cogX / endPointX)

        cogNormalForce =
            centerOfGravityForce * cos (degrees model.barAngle)

        endPointNormalForce =
            endPointGravityForce * cos (degrees model.barAngle)

        cogNormalForceEndPoint =
            ( getX centerOfGravityPoint
            , getY centerOfGravityPoint - (cogNormalForce * model.forceToLengthFactor)
            )

        cogGravityForceEndPoint =
            ( getX centerOfGravityPoint - (centerOfGravityForce * model.forceToLengthFactor * sin (degrees model.barAngle))
            , getY cogNormalForceEndPoint
            )

        handlePoint =
            ( originX + model.barLength, originY )

        handleNormalForceEndPoint =
            ( getX handlePoint
            , getY handlePoint - (endPointNormalForce * model.forceToLengthFactor)
            )

        handleGravityForceEndPoint =
            ( getX handlePoint - (endPointGravityForce * model.forceToLengthFactor * sin (degrees model.barAngle))
            , getY handleNormalForceEndPoint
            )

        barGeo =
            BarGeometry
                shape
                centerOfGravityPoint
                handlePoint
                cogGravityForceEndPoint
                cogNormalForceEndPoint
                handleGravityForceEndPoint
                handleNormalForceEndPoint
                (model.barWeight + model.extraWeight)
                endPointGravityForce
                cogNormalForce
                endPointNormalForce
                weightShape
    in
    rotateBar model.barAngle model.fulcrum barGeo


rotateBar : Float -> Point -> BarGeometry -> BarGeometry
rotateBar angle origin barGeo =
    BarGeometry
        (List.map (rotatePoint angle origin) barGeo.shape)
        (rotatePoint angle origin barGeo.centerOfGravityPoint)
        (rotatePoint angle origin barGeo.handlePoint)
        (rotatePoint angle origin barGeo.cogGravityForceEndPoint)
        (rotatePoint angle origin barGeo.cogNormalForceEndPoint)
        (rotatePoint angle origin barGeo.handleGravityForceEndPoint)
        (rotatePoint angle origin barGeo.handleNormalForceEndPoint)
        barGeo.cogGravityForce
        barGeo.endPointGravityForce
        barGeo.cogNormalForce
        barGeo.endPointNormalForce
        (List.map (rotatePoint angle origin) barGeo.weightShape)


mapPointToSvgCoordinates : Point -> Point
mapPointToSvgCoordinates point =
    let
        x =
            getX point

        y =
            getY point
    in
    ( x + xPad, viewHeight - y - yPad )


mapToSvgCoordinates : BarGeometry -> BarGeometry
mapToSvgCoordinates barGeo =
    BarGeometry
        (List.map mapPointToSvgCoordinates barGeo.shape)
        (mapPointToSvgCoordinates barGeo.centerOfGravityPoint)
        (mapPointToSvgCoordinates barGeo.handlePoint)
        (mapPointToSvgCoordinates barGeo.cogGravityForceEndPoint)
        (mapPointToSvgCoordinates barGeo.cogNormalForceEndPoint)
        (mapPointToSvgCoordinates barGeo.handleGravityForceEndPoint)
        (mapPointToSvgCoordinates barGeo.handleNormalForceEndPoint)
        barGeo.cogGravityForce
        barGeo.endPointGravityForce
        barGeo.cogNormalForce
        barGeo.endPointNormalForce
        (List.map mapPointToSvgCoordinates barGeo.weightShape)



-- UPDATE


update : (Msg Bool) -> Model -> ( Model, Cmd (Msg Bool) )
update msg model =
    case msg of
        Angle angle ->
            ( { model | barAngle = Maybe.withDefault 10.0 (Just angle) }, Cmd.none )

        AngleStr angle ->
            ( { model | barAngle = Maybe.withDefault 10.0 (String.toFloat angle) }, Cmd.none )

        BarWeightChanged weight ->
            ( { model | barWeight = Maybe.withDefault 45.0 (String.toFloat weight) }, Cmd.none )

        ExtraWeightChanged extraWeight_ ->
            ( { model | extraWeight = Maybe.withDefault 0.0 (String.toFloat extraWeight_) }, Cmd.none )

        NormalForceOptionChanged b ->
            ( { model | showNormalForceVector = b}, Cmd.none )

        GravityForceOptionChanged b ->
            ( { model | showGravityForceVector = b}, Cmd.none )

        BarLengthChanged lengthStr ->
            ( { model | barLength = stringToFloat lengthStr }, Cmd.none )


        ForceToLenFactorChanged factorStr ->
            ( { model | forceToLengthFactor = stringToFloat factorStr }, Cmd.none )



-- SUBSCRIPTIONS


stringToFloat strval =
    case String.toFloat strval of
        Just val ->
            val

        Nothing ->
            0.0


subscriptions : Model -> Sub (Msg m)
subscriptions _ =
    Sub.none


tuple2str : Point -> String
tuple2str tpl =
    String.fromFloat (getX tpl) ++ "," ++ String.fromFloat (getY tpl)


path2svgPath : Path -> String
path2svgPath path =
    String.join " " (List.map tuple2str path)



-- VIEW


boolToString : Maybe Bool -> String
boolToString boolean =
    case boolean of
        Just True ->
            "True"

        Just False ->
            "False"

        Nothing ->
            "unknown"

view model =
    let
        barGeo : BarGeometry
        barGeo =
            mapToSvgCoordinates (calculateBarGeometry model)
    in

        El.layout [ El.width El.fill, El.height El.fill, padding 5, spacing 5 ]
        --colStuff
        (c1 model)


c1 model =
    column [ bCol 1, El.width El.fill, El.height El.fill , padding 5, spacing 5]
        [ r1_c1 model, r2_c1 model]


r1_c1 model =
    row [ bCol 2, El.width El.fill, El.height (fillPortion 10)  , padding 5, spacing 5]
        [ c1_r1_c1 model, c2_r1_c1 model]



r2_c1 model =
    row [ bCol 9, alignBottom, El.width El.fill, El.height (fillPortion 1) , padding 5, spacing 5]
        [ text1 model, text2 model, text3 model]


c1_r1_c1 model =
    column [ bCol 3, El.width (fillPortion 9), alignTop , padding 5, spacing 5, El.height El.fill]
        [ r1_c1_r1_c1 model, r2_c1_r1_c1 model]

c2_r1_c1 : Model -> Element (Msg m)
c2_r1_c1 model =
    let
        -- slider : FloatValue { min:Float, max:Float, message:(Float -> m), label:String} -> Element msg
        slider = FloatValue { min = 0, max = 90, message = Angle, label = "" }
    in
        column [ bCol 4, El.width (fillPortion 1) , padding 5, spacing 5, El.height El.fill]
        [ drawInput slider model.barAngle Vertical]


aSlider =
    el [ centerY, centerX , padding 5, spacing 5, bCol 7] 
        (el [ centerY , padding 5, spacing 5] (El.text "aSlider"))


r1_c1_r1_c1 model =
    row [ bCol 5, alignTop , padding 5, spacing 25, El.width El.fill]
        [ c1_r1_c1_r1_c1 model, c2_r1_c1_r1_c1 model]


r2_c1_r1_c1 model =
    row [ bCol 6, El.height El.fill, El.width El.fill, alignBottom , padding 5, spacing 5]
        [ aSvg model ]


aSvg model =
    let
        barGeo : BarGeometry
        barGeo =
            mapToSvgCoordinates (calculateBarGeometry model)
    in
        el [El.height El.fill, El.width El.fill, padding 5, spacing 5] 
        -- (El.text "aSvg")
        (El.html (drawFulBar model barGeo))

c1_r1_c1_r1_c1 model =
    column [ bCol 7 , padding 5, spacing 5, 
    El.width (El.fill
                        |> El.maximum 200
                        |> El.minimum 100)]
        [ aSlider4 model]

c2_r1_c1_r1_c1 model =
    column [ bCol 8 , padding 5, spacing 5, alignRight]
        [ checkbox1 model, checkbox2 model ]

{--
aSlider1 =
    let
        slider = IntValue { min = 25, max = 90, step=5,  message = BarWeightChanged, label = "" }
    in
        column [ bCol 4, El.width (fillPortion 1) , padding 5, spacing 5, El.height El.fill]
        [ drawInput slider 45 Horizontal]
        --[ drawInput slider 4 Vertical]


aSlider2 =
    let
        -- slider = IntValue { min = 0, max = 60, step=1,  message = ExtraWeightChanged, label = "" }
        slider = FloatValue { min = 0, max = 60,  message = ExtraWeightChanged, label = "" }
    in
        column [ bCol 4, El.width (fillPortion 1) , padding 5, spacing 5, El.height El.fill]
        [ drawInput slider 0 Horizontal]
        --[ drawInput slider 0 Vertical]

--}
aSlider3 model = 
        column [ bCol 20, El.width (fillPortion 1) , padding 5, spacing 5, 
                El.height (El.fill
                        |> El.maximum 300
                        |> El.minimum 100)]
        [ El.html (input
            [ type_ "range"
            , Attrs.min "0"
            , Attrs.max "90"
            , value <| String.fromFloat model.barAngle
            , onInput AngleStr
            ]
            [])
        ]

aSlider4 model = 
        column [ bCol 21, El.width El.fill, padding 5, spacing 5, 
                El.height El.fill]
                        
        [ Input.text [] {
            onChange = BarWeightChanged
            , text = String.fromFloat model.barWeight
            , placeholder = Just <| Input.placeholder [] <| El.text "Type here"
            , label = Input.labelLeft [] <| El.text "Bar Weight"
        }
         ,
          Input.text [] {
            onChange = ExtraWeightChanged
            , text = String.fromFloat model.extraWeight
            , placeholder = Just <| Input.placeholder [] <| El.text "Type here"
            , label = Input.labelLeft [] <| El.text "Extra Weight"
        }

        ]

checkbox1 : Model -> Element (Msg Bool)
checkbox1 model =
    drawCheckbox "show normal force vector" NormalForceOptionChanged model.showNormalForceVector

checkbox2 : Model -> Element (Msg Bool)
checkbox2 model =
    drawCheckbox "show gravity force vector" GravityForceOptionChanged model.showGravityForceVector





text1 model =
    el [] (El.text ("angle: " ++ (String.fromFloat <| model.barAngle)))


text2 model =
    el [] (El.text "aText2")


text3 model =
    el [] (El.text "aText3")



drawFulBar: Model -> BarGeometry -> Html (Msg m)
drawFulBar model barGeo =
     svg
        [ Svg.Attributes.width "600"
        , Svg.Attributes.height "600"
        , viewBox ("0 0 " ++ (String.fromFloat viewWidth) ++ " " ++ (String.fromFloat viewHeight))
        ]
        [ defs []
            [ marker
                [ id "arrowhead"
                , viewBox "0 0 10 10"
                , markerUnits "strokeWidth"
                , markerWidth "10"
                , markerHeight "10"
                , refX "1"
                , refY "5"
                , orient "auto"
                ]
                [ path [ d "M 0 0 L 10 5 L 0 10 z", Svg.Attributes.fill "black" ] [] ]
            ]
        , drawBar barGeo.shape barGeo.weightShape
        , if model.showGravityForceVector == True then
            drawForceVector 
                barGeo.centerOfGravityPoint 
                barGeo.cogGravityForceEndPoint 
                "red" 
                (Round.round 2 barGeo.cogGravityForce ++ " lbs")

            else
            div [] []
        , if model.showNormalForceVector == True then
            drawForceVector 
                barGeo.centerOfGravityPoint 
                barGeo.cogNormalForceEndPoint 
                "blue" 
                (Round.round 2 barGeo.cogNormalForce ++ " lbs")

            else
            div [] []
        , if model.showGravityForceVector == True then
            drawForceVector 
                barGeo.handlePoint 
                barGeo.handleGravityForceEndPoint 
                "green" 
                (Round.round 2 barGeo.endPointGravityForce ++ " lbs")

            else
            div [] []
        , if model.showNormalForceVector == True then
            drawForceVector 
                barGeo.handlePoint 
                barGeo.handleNormalForceEndPoint 
                "purple" 
                (Round.round 2 barGeo.endPointNormalForce ++ " lbs")
            else
            div [] []
        ]



drawBar shape weight =
    g []
        [ polygon [ Svg.Attributes.fill "None", stroke "black", points (path2svgPath shape) ] []
        , polygon [ Svg.Attributes.fill "black", stroke "black", points (path2svgPath weight) ] []
        ]


drawForceVector start end color_ label_ =
    g []
        [ line
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
            , x (String.fromFloat (getX end))
            , y (String.fromFloat (getY end + 20))
            , Svg.Attributes.fill color_
            ]
            [ Html.text label_ ]
        ]
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

bCol: Int -> El.Attr decorative msg
bCol i =
    let
        r = 2 * i * 5
        g = 4 * i * 5
        b = 3 * i * 5
    in
    -- Background.color (rgb255 r g b)
    Background.color color1.col10



