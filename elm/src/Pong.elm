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


type Direction
    = Up
    | Down
    | Neutral


type alias Model =
    { ball : Ball
    , posPlayer1 : Float
    , posPlayer2 : Float
    , player1Direction : Direction
    , player2Direction : Direction
    }


type alias Ball =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


delta =
    10


init : Model
init =
    { ball = Ball 400 300 0 0
    , posPlayer1 = 300
    , posPlayer2 = 300
    , player1Direction = Neutral
    , player2Direction = Neutral
    }


xPlayer1 =
    30


xPlayer2 =
    800 - xPlayer1



--- Update


type Msg
    = MoveP1 Direction
    | MoveP2 Direction
    | Tick Float
    | TogglePause
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        MoveP1 dir ->
            ( { m | posPlayer1 = m.posPlayer1 + changePos dir }
            , Cmd.none
            )

        MoveP2 dir ->
            ( { m | posPlayer2 = m.posPlayer2 + changePos dir }
            , Cmd.none
            )

        Tick dt ->
            ( { m | ball = updatePhysics (dt / 1000) m }
            , Cmd.none
            )

        TogglePause ->
            let
                newBall =
                    { x = 400
                    , y = 300
                    , vx = 200
                    , vy = -200
                    }
            in
            ( { m | ball = newBall }, Cmd.none )

        _ ->
            ( m, Cmd.none )


changePos : Direction -> Float
changePos dir =
    case dir of
        Up ->
            -delta

        Down ->
            delta

        Neutral ->
            0


updatePhysics : Float -> Model -> Ball
updatePhysics dt m =
    let
        { x, y, vx, vy } =
            m.ball

        x_ =
            x + vx * dt

        y_ =
            y + vy * dt

        ( vx_, vy_ ) =
            if (x < -120 && vx < 0) || (x > 1000 && vx > 0) then
                ( -vx, vy )

            else if (y < 20 && vy < 0) || (y > 580 && vy > 0) then
                ( vx, -vy )

            else if hasCollision m then
                ( -vx, vy )

            else
                ( vx, vy )
    in
    Ball x_ y_ vx_ vy_


hasCollision : Model -> Bool
hasCollision m =
    let
        { x, y, vx } =
            m.ball

        collidePlayer yp =
            y > (yp - 50.0) && y < (yp + 50.0)

        collideP1 =
            (x < 60) && (vx < 0) && collidePlayer m.posPlayer1

        collideP2 =
            (x > 740) && (vx > 0) && collidePlayer m.posPlayer2
    in
    collideP1 || collideP2



--- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyDown (Decode.map (key True) keyCode)
        , onKeyUp (Decode.map (key False) keyCode)
        ]



--- View


view : Model -> Html Msg
view m =
    div
        [ style "background" "white"
        , style "width" "100%"
        , style "height" "100vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "overflow" "hidden"
        , style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttrs.width "800px"
            , SvgAttrs.height "600px"
            , style "background" "#000000"
            ]
            [ player xPlayer1 m.posPlayer1
            , player xPlayer2 m.posPlayer2
            , ball m.ball.x m.ball.y
            ]
        ]


player : Float -> Float -> Html Msg
player x y =
    -- <rect width="20px" height="100px" x="..." y="..."/>
    Svg.rect
        [ SvgAttrs.width "20px"
        , SvgAttrs.height "100px"
        , SvgAttrs.fill "#00ff00"
        , SvgAttrs.x (String.fromFloat (x - 10))
        , SvgAttrs.y (String.fromFloat (y - 50))
        ]
        []


ball : Float -> Float -> Html Msg
ball x y =
    Svg.circle
        [ SvgAttrs.r "20px"
        , SvgAttrs.fill "#00ff00"
        , SvgAttrs.cx (String.fromFloat x)
        , SvgAttrs.cy (String.fromFloat y)
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


key : Bool -> Int -> Msg
key isDown keycode =
    case keycode of
        -- "w"
        87 ->
            if isDown then
                MoveP1 Up

            else
                MoveP1 Neutral

        -- "s"
        83 ->
            MoveP1 Down

        -- up arrow
        38 ->
            MoveP2 Up

        -- down arrow
        40 ->
            MoveP2 Down

        -- space
        32 ->
            TogglePause

        _ ->
            NoOp
