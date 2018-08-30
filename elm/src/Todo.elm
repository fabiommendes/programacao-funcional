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
    { todos : List String, current : String }


init : Model
init =
    { todos = [], current = "" }



-- MESSAGES ----------------------------------------------


type Msg
    = Clear
    | Add
    | Current String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Clear ->
            { todos = []
            , current = ""
            }

        Add ->
            { todos = m.current :: m.todos
            , current = ""
            }

        Current st ->
            { m | current = st }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "TODOS" ]
        , showTodos m.todos
        , inputElem m
        , button [ onClick Add ] [ text "Add" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


showTodos m =
    ul [] (List.map showTodo m)


showTodo st =
    li [] [ text st ]


inputElem m =
    input
        [ placeholder "What to do?"
        , onInput Current
        , value m.current
        ]
        []
