module BoundedNumericValue exposing (..)

import Browser
import Element exposing (Element, behindContent, centerX, centerY, column, el, fill, height, padding, px, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
-- import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main : Program () Model (Msg Bool)
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { value1 : Float, value2 : Float, checkbox1 : Bool}


init : Model
init =
    { value1 = 2 , value2 = 3.1459, checkbox1 = True}


update : (Msg Bool) -> Model -> Model
update msg model =
    case msg of
        AdjustValue1 v ->
            { model | value1 = v }
        AdjustValue2 v ->
            { model | value2 = v }
        Checkbox1Changed b ->
            { model | checkbox1 = b }

type Msg m
    = AdjustValue1 Float
    | AdjustValue2 Float
    | Checkbox1Changed m


type BoundedNumericValue m
    = IntValue { min : Int, max : Int, step : Int, message : Float -> m, label : String }
    | FloatValue { min : Float, max : Float, message : Float -> m, label : String }


type Orientation
    = Vertical
    | Horizontal


drawInput : BoundedNumericValue msg -> Float -> Orientation -> Element msg
drawInput boundedValue value_ orientation =
    let
        -- label_ =
        --     case boundedValue of
        --         IntValue { min, max, step, message, label } ->
        --             text (label ++ String.fromFloat value_)

        --         FloatValue { min, max, message, label } ->
        --             text (label ++ String.fromFloat value_)

        { min_, max_, step_, message_ } =
            case boundedValue of
                IntValue { min, max, step, message, label } ->
                    { min_ = toFloat <| min, max_ = toFloat <| max, step_ = Just (toFloat <| step), message_ = message }

                FloatValue { min, max, message, label } ->
                    { min_ = min, max_ = max, step_ = Nothing, message_ = message }

        { sliderHeight_, sliderWidth_, trackHeight_, trackWidth_ } =
            case orientation of
                Vertical ->
                    { sliderHeight_ = px 300, sliderWidth_ = px 30, trackHeight_ = fill, trackWidth_ = px 5 }

                Horizontal ->
                    { sliderHeight_ = px 30, sliderWidth_ = px 300, trackHeight_ = px 5, trackWidth_ = px 300 }
    in
    Input.slider
        [ height sliderHeight_
        , width sliderWidth_
        , centerX
        , centerY
        , behindContent <|
            -- Slider track
            el
                [ width trackWidth_
                , height trackHeight_
                , centerX
                , centerY
                , Background.color color.lightGrey
                , Border.rounded 6
                ]
                Element.none
        ]
        { onChange = message_
        , label = Input.labelHidden (String.fromFloat value_)
        , min = min_
        , max = max_
        , step = step_
        , value = value_
        , thumb = Input.defaultThumb
        }


view : Model -> Html (Msg Bool)
view model =
    Element.layout [ width fill, height fill ] <|
        column
            [ padding 50 ]
            [ drawInput (IntValue { min = 0, max = 400, step = 1, message = AdjustValue1, label = "Integer Value 1: " }) model.value1 Horizontal
            , drawInput (FloatValue { min = 0, max = 10, message = AdjustValue2, label = "Real Value 2: " }) model.value2 Horizontal
            , drawCheckbox "show normal force vector" Checkbox1Changed model.checkbox1
            ]

drawCheckbox : String -> (Bool -> msg) -> Bool -> Element msg
drawCheckbox label_ message_ value_ =
    Input.checkbox []
        { onChange = message_
        , icon = Input.defaultCheckbox
        , checked = value_
        , label = Input.labelRight [] <| text label_
        }

color : { blue : Element.Color, darkCharcoal : Element.Color, lightBlue : Element.Color, lightGrey : Element.Color, white : Element.Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }
