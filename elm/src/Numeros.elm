module HelloName exposing (main)

{-| Pergunta um número e diz várias coisas
sobre o número
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { input : String
    , value : Int
    }


init : Model
init =
    { input = "", value = 0 }



-- UPDATE


type Msg
    = NoOp
    | OnUpdate String
    | OnSend


update : Msg -> Model -> Model
update msg m =
    case msg of
        NoOp ->
            m

        OnUpdate st ->
            { m | input = st }

        OnSend ->
            { input = ""
            , value =
                String.toInt m.input
                    |> Maybe.withDefault 0
            }



-- VIEW


view : Model -> Html Msg
view m =
    let
        value =
            String.fromInt (fat m.value)
    in
    div []
        [ div []
            [ Html.form [ onSubmit OnSend ]
                [ inputElement m
                , inputButton
                ]
            , values m.value
            ]
        ]


inputElement : Model -> Html Msg
inputElement m =
    input
        [ value m.input
        , onInput OnUpdate
        , placeholder "Digite um número"
        ]
        []


values : Int -> Html Msg
values n =
    div []
        [ div []
            [ text ("Fat: " ++ String.fromInt (fat n)) ]
        , viewFibs n
        , div [] [ text ("Exp: " ++ String.fromFloat (exp (toFloat n))) ]
        ]


viewFibs : Int -> Html Msg
viewFibs n =
    div []
        [ text "Sequencia Fibonacci"
        , viewList (fibList n)
        ]


viewList : List Int -> Html Msg
viewList lst =
    let
        makeLi n =
            li [] [ text (String.fromInt n) ]

        children =
            List.map makeLi lst
    in
    ol [] children


inputButton =
    input
        [ type_ "submit"
        , value "Send"
        ]
        []


fat : Int -> Int
fat n =
    let
        fat2 m acc =
            if m == 0 then
                acc

            else
                fat2 (m - 1) (acc * m)
    in
    fat2 n 1


fibList : Int -> List Int
fibList n =
    List.reverse (fibAcc n [ 1, 1 ])


fibNext lst =
    case lst of
        [] ->
            [ 1 ]

        [ x ] ->
            [ 1, x ]

        x :: y :: tail ->
            (x + y) :: lst


fibAcc n acc =
    if n == 0 then
        acc

    else
        fibAcc (n - 1) (fibNext acc)


exp : Float -> Float
exp x =
    let
        term : Int -> Float
        term n =
            x ^ toFloat n / toFloat (fat n)
    in
    List.sum <|
        List.map term (List.range 0 30)



-- DANGER: GARBAGE ZONE
-- fib : Int -> Int
-- fib m =
--     let
--         fibAcc n x y =
--             if n == 0 then
--                 y
--             else
--                 fibAcc (n - 1) y (x + y)
--     in
--     fibAcc m 0 1
