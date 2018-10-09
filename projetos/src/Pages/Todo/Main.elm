module Pages.Todo exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (updateAt)


main =
    Browser.sandbox
        { view = view
        , update = update
        , init = example
        }



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { todos : List Todo, current : String }


type alias Todo =
    { text : String
    , done : Bool
    }


init : Model
init =
    { todos = [], current = "" }


mkTodo : String -> Todo
mkTodo st =
    Todo st False


done : Todo -> Todo
done x =
    { x | done = True }


toggle : Todo -> Todo
toggle x =
    { x | done = not x.done }



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = Clear
    | Add
    | Current String
    | Toggle Int


update : Msg -> Model -> Model
update msg m =
    case msg of
        Clear ->
            { todos = List.filter (not << .done) m.todos
            , current = ""
            }

        Add ->
            { todos = mkTodo m.current :: m.todos
            , current = ""
            }

        Toggle i ->
            { m | todos = updateAt i toggle m.todos }

        Current st ->
            { m | current = st }



--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "TODOS" ]
        , viewTodos m.todos
        , Html.form [ onSubmit Add ] [ inputElem m ]
        , button [ onClick Add ] [ text "Add" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


viewTodos m =
    let
        showTodo i todo =
            let
                styleProp =
                    if todo.done then
                        style "text-decoration" "line-through"

                    else
                        style "text-decoration" "initial"
            in
            li []
                [ input [ type_ "checkbox", onClick (Toggle i), checked todo.done ] []
                , span [ styleProp ] [ text todo.text ]
                ]
    in
    ul [] (List.indexedMap showTodo m)


inputElem m =
    input
        [ placeholder "What to do?"
        , onInput Current
        , value m.current
        ]
        []



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | todos =
            [ mkTodo "Learn Haskell"
            , mkTodo "Learn Elm"
            , mkTodo "Conquer the world!"
            ]
    }
