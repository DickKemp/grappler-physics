module Main exposing (main, viewHeight)

-- visualize the physics of the grappler
--
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)
-- import Maybe

import Browser
import Dict
import Html exposing (Html, br, div, fieldset, input, label)
import Html.Attributes as Attrs exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
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
    180.0 * 2.5


barWidth : Float
barWidth =
    2 * 2.5


barWeight : Float
barWeight =
    45.0


fulcrum : Point
fulcrum =
    ( 50, 50 )


forceToLengthFactor : Float
forceToLengthFactor =
    2.2


type alias Model =
    { barLength : Float
    , barWeight : Float
    , extraWeight : Float
    , barAngle : Float
    , extraWeightDistanceFromFulcrum : Float
    , displayOptions : Dict.Dict DisplayOptions Bool
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


type alias DisplayOptions =
    String


type Msg
    = Angle String
    | ToggleDisplayOptions DisplayOptions
    | ForceToLenFactorChanged String
    | BarLengthChanged String
    | BarWeightChanged String
    | ExtraWeightChanged String


toggle : comparable -> Dict.Dict comparable Bool -> Dict.Dict comparable Bool
toggle key dict =
    Dict.update key
        (\oldValue ->
            case oldValue of
                Just value ->
                    Just <| not value

                Nothing ->
                    Nothing
        )
        dict


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        displayOptions =
            Dict.fromList
                [ ( showCoGForceVector, False )
                , ( showCogNormalForceVector, False )
                , ( showHandleGravityForceVector, False )
                , ( showHandleNormalForceVector, False )
                , ( showLabels, False )
                ]
    in
    ( Model barLength
        barWeight
        -- extra weight added to bar
        0.0
        -- bar angle in degrees
        0.0
        -- extraWeightDistanceFromFulcrum
        (barLength - (barLength / 8))
        displayOptions
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Angle angle ->
            ( { model | barAngle = Maybe.withDefault 10.0 (String.toFloat angle) }, Cmd.none )

        ToggleDisplayOptions options ->
            ( { model | displayOptions = toggle options model.displayOptions }, Cmd.none )

        BarLengthChanged lengthStr ->
            ( { model | barLength = stringToFloat lengthStr }, Cmd.none )

        BarWeightChanged weightStr ->
            ( { model | barWeight = stringToFloat weightStr }, Cmd.none )

        ExtraWeightChanged extraWeightStr ->
            ( { model | extraWeight = stringToFloat extraWeightStr }, Cmd.none )

        ForceToLenFactorChanged factorStr ->
            ( { model | forceToLengthFactor = stringToFloat factorStr }, Cmd.none )



-- SUBSCRIPTIONS


stringToFloat strval =
    case String.toFloat strval of
        Just val ->
            val

        Nothing ->
            0.0


subscriptions : Model -> Sub Msg
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


optionCheckBox option model =
    div
        []
        [ input
            [ type_ "checkbox"
            , checked <| Maybe.withDefault False (Dict.get option model.displayOptions)
            , onClick <| ToggleDisplayOptions option
            ]
            []
        , text <| " " ++ option 
        -- , text <| boolToString <| Dict.get option model.displayOptions
        ]


isOptionSet option model =
    let
        result =
            Dict.get option model.displayOptions
    in
    case result of
        Just val ->
            val

        Nothing ->
            False


view : Model -> Html Msg
view model =
    let
        barGeo : BarGeometry
        barGeo =
            mapToSvgCoordinates (calculateBarGeometry model)
    in
    div
        []
        [ input
            [ type_ "range"
            , Attrs.min "0"
            , Attrs.max "90"
            , value <| String.fromFloat model.barAngle
            , onInput Angle
            ]
            []
        , text <| String.fromFloat model.barAngle
        , br [] []
        , fieldset []
            [ optionCheckBox showCoGForceVector model
            , optionCheckBox showCogNormalForceVector model
            , optionCheckBox showHandleGravityForceVector model
            , optionCheckBox showHandleNormalForceVector model
            ]
        , br [] []
        , input [ placeholder "factor", value (String.fromFloat model.forceToLengthFactor), onInput ForceToLenFactorChanged ] []
        , input [ placeholder "barLength", value (String.fromFloat model.barLength), onInput BarLengthChanged ] []
        , input [ placeholder "barWeight", value (String.fromFloat model.barWeight), onInput BarWeightChanged ] []
        , input [ placeholder "extraWeight", value (String.fromFloat model.extraWeight), onInput ExtraWeightChanged ] []
        , br [] []

        -- , label [] [ text ("cog force: " ++ String.fromFloat barGeo.cogGravityForce) ]
        -- , br [] []
        -- , label [] [ text ("handle force: " ++ String.fromFloat barGeo.endPointGravityForce) ]
        -- , br [] []
        , svg
            [ width "800"
            , height "800"
            , viewBox "0 0 viewWidth viewHeight"
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
                    [ path [ d "M 0 0 L 10 5 L 0 10 z", fill "black" ] [] ]
                ]
            , drawBar barGeo.shape barGeo.weightShape
            , if isOptionSet showCoGForceVector model then
                drawForceVector 
                    barGeo.centerOfGravityPoint 
                    barGeo.cogGravityForceEndPoint 
                    "red" 
                    (Round.round 2 barGeo.cogGravityForce ++ " lbs")

              else
                div [] []
            , if isOptionSet showCogNormalForceVector model then
                drawForceVector 
                    barGeo.centerOfGravityPoint 
                    barGeo.cogNormalForceEndPoint 
                    "blue" 
                    (Round.round 2 barGeo.cogNormalForce ++ " lbs")

              else
                div [] []
            , if isOptionSet showHandleGravityForceVector model then
                drawForceVector 
                    barGeo.handlePoint 
                    barGeo.handleGravityForceEndPoint 
                    "green" 
                    (Round.round 2 barGeo.endPointGravityForce ++ " lbs")

              else
                div [] []
            , if isOptionSet showHandleNormalForceVector model then
                drawForceVector 
                    barGeo.handlePoint 
                    barGeo.handleNormalForceEndPoint 
                    "purple" 
                    (Round.round 2 barGeo.endPointNormalForce ++ " lbs")

              else
                div [] []
            , if isOptionSet showLabels model then
                div [] []

              else
                div [] []
            ]
        ]


drawBar shape weight =
    g []
        [ polygon [ fill "None", stroke "black", points (path2svgPath shape) ] []
        , polygon [ fill "black", stroke "black", points (path2svgPath weight) ] []
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
            , fill color_
            ]
            [ text label_ ]
        ]
