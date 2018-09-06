module HelloName exposing (main)

{-| Pergunta o nome do usuário e diz oi!
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
    , value : String
    }


init : Model
init =
    { input = "", value = "" }



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
            { input = "", value = m.input }



-- VIEW


view : Model -> Html Msg
view m =
    div []
        [ Html.form [ onSubmit OnSend ]
            [ inputElement m
            , inputButton
            ]
        , div [] [ text ("Hello " ++ m.value) ]
        ]


inputElement m =
    input
        [ value m.input
        , onInput OnUpdate
        , placeholder "Digite seu nome"
        ]
        []


inputButton =
    input
        [ type_ "submit"
        , value "Send"
        ]
        []
