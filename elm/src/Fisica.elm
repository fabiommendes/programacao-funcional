module Fisica exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Svg
import Svg.Attributes as SvgAttrs



--- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


delta =
    10

init : Model
init =
    Model 300 400 200 -500



--- Update


type Msg
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | Tick Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        MoveUp ->
            ( { m | y = m.y - delta }, Cmd.none )

        MoveDown ->
            ( { m | y = m.y + delta }, Cmd.none )

        MoveLeft ->
            ( { m | x = m.x - delta }, Cmd.none )

        MoveRight ->
            ( { m | x = m.x + delta }, Cmd.none )

        Tick deltaMs ->
            let
                dt = deltaMs / 1000
                g = 500
                x = m.x + m.vx * dt
                y = m.y + m.vy * dt
                vy = m.vy + g * dt
            in
            ( Model x y m.vx vy, Cmd.none )

        NoOp ->
            ( m, Cmd.none )



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map key keyCode)
        ]



--- View


view : Model -> Html Msg
view { x, y } =
    div [ style "background" "green" ]
        [ Svg.svg
            [ SvgAttrs.width "800px"
            , SvgAttrs.height "600px"
            , style "background" "#4466FF"
            ]
            [ box x y
            ]
        ]


box : Float -> Float -> Html Msg
box x y =
    Svg.rect
        [ SvgAttrs.width "100"
        , SvgAttrs.height "100"
        , SvgAttrs.fill "#ff0000"
        , SvgAttrs.stroke "#000000"
        , SvgAttrs.strokeWidth "0.5"
        , SvgAttrs.x (String.fromFloat x)
        , SvgAttrs.y (String.fromFloat y)
        ]
        []



--- Main


main : Program Value Model Msg
main =
    Browser.element
        { init = \value -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


key : Int -> Msg
key keycode =
    case keycode of
        37 ->
            MoveLeft

        39 ->
            MoveRight

        40 ->
            MoveDown

        38 ->
            MoveUp

        _ ->
            NoOp
