module App exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { view = view
        , update = update
        , init = init
        }



-- MODEL --------------------------------------------------


type alias Model =
    String


init : Model
init =
    "Elm"



-- MESSAGES ----------------------------------------------


type Msg
    = Clear
    | Concat String
    | Prepend String
    | Replace String
    | NoOp


update : Msg -> Model -> Model
update msg m =
    case msg of
        Clear ->
            ""

        Concat st ->
            m ++ " " ++ st

        Prepend st ->
            st ++ " " ++ m

        Replace st ->
            st

        NoOp ->
            m



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text m ]
        , input [ placeholder "Type here", onInput Replace ] []
        , button [ onClick (Concat "hey!") ] [ text "hey" ]
        , button [ onClick (Prepend "ho!") ] [ text "ho" ]
        , button [ onClick (Concat "lets go!") ] [ text "lets go" ]
        , button [ onClick Clear ] [ text "clear" ]
        ]
