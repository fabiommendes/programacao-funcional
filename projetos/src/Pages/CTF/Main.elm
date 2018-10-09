module Pages.CTF exposing (Model, init, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Icon exposing (..)
import Utils exposing (..)


main =
    Browser.sandbox
        { init = example
        , view = view
        , update = update
        }



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { flags : List Flag
    , expanded : Int
    , response : String
    }


type alias Flag =
    { title : String
    , description : String
    , value : Int
    , captured : Bool
    }


init : Model
init =
    { flags = [], expanded = 0, response = "" }


{-| Create simple flag element
-}
flag title descr value =
    Flag title descr value False



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Expand Int
    | UpdateResponse Int String
    | SendResponse Int


update : Msg -> Model -> Model
update msg m =
    case msg of
        Expand i ->
            { m | expanded = i }

        UpdateResponse i st ->
            { m | response = st }

        SendResponse i ->
            { m | response = "" }

        _ ->
            m



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "CTF Competition" ]
        , ulIndexedMap (viewFlag m.response m.expanded) m.flags
        ]


viewFlag : String -> Int -> Int -> Flag -> Html Msg
viewFlag response expanded i obj =
    let
        body =
            if i == expanded then
                flagChildren response i obj

            else
                []
    in
    div [] (flagTitle i obj :: body)


flagTitle i obj =
    h2 [ onClick (Expand i) ]
        [ text obj.title
        , text <| " (" ++ String.fromInt obj.value ++ "pts)"
        ]


flagChildren response i obj =
    [ p [] [ text obj.description ]
    , div []
        [ text "Answer: "
        , input [ placeholder "42", onInput (UpdateResponse i), value response ] []
        , button [ onClick (SendResponse i) ] [ text "Send" ]
        ]
    ]



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | flags =
            [ flag "Fibonacci" "Find some fibonacci numbers" 1
            , flag "Collatz" "Sequence of numbers" 2
            , flag "Factorial" "Blah" 2
            ]
    }
