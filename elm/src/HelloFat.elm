module HelloName exposing (main)

{-| Pergunta um número e diz o fatorial
-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


css =
    """
div {
    font-family: Roboto;
    font-size: 25px;
    color: magenta;
}
"""


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
        [ Html.node "style" [] [ text css ]
        , div []
            [ Html.form [ onSubmit OnSend ]
                [ inputElement m
                , inputButton
                ]
            , div [] [ text ("Fat: " ++ value) ]
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


inputButton =
    input
        [ type_ "submit"
        , value "Send"
        ]
        []


fat n =
    let
        fat2 m acc =
            if m == 0 then
                acc

            else
                fat2 (m - 1) (acc * m)
    in
    fat2 n 1


{-| Tradução js:

    acc = 1;

    while (true) {
        if (n === 0){
            return acc;
        }
        else {
            n = n - 1;
            acc = acc * n;
        }
    }

-}
fatAcc n acc =
    if n == 0 then
        acc

    else
        fatAcc (n - 1) (acc * n)


fatAlt n =
    if n == 0 then
        1

    else
        n * fatAlt (n - 1)
