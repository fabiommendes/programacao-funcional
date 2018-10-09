module Pages.Pastes exposing (Model, Msg, init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (updateAt)
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
    { lines : List Line
    , input : String
    , author : String
    }


type alias Line =
    { text : String
    , comments : List Comment
    , expanded : Bool
    }


type alias Comment =
    { text : String
    , author : String
    }


init : Model
init =
    { lines = [], input = "", author = "me!" }


line : String -> Line
line title =
    Line title [] False


addComment : Int -> Comment -> List Line -> List Line
addComment i comment lst =
    updateAt i (\ln -> { ln | comments = ln.comments ++ [ comment ] }) lst


toggleLine : Int -> List Line -> List Line
toggleLine i lst =
    updateAt i (\ln -> { ln | expanded = not ln.expanded }) lst



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Add Int
    | Toggle Int
    | Input String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Add i ->
            let
                x =
                    { text = m.input, author = m.author }
            in
            { m | lines = addComment i x m.lines, input = "" }

        Input st ->
            { m | input = st }

        Toggle i ->
            { m | lines = toggleLine i m.lines }

        _ ->
            m



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "Code review" ]
        , ulIndexedMap (viewLine m) m.lines
        ]


viewLine : Model -> Int -> Line -> Html Msg
viewLine m i ln =
    div []
        (pre [ onClick (Toggle i) ] [ code [] [ text ln.text ] ]
            :: (if ln.expanded then
                    [ viewComments m i ln.comments ]

                else
                    []
               )
        )


viewComments : Model -> Int -> List Comment -> Html Msg
viewComments m i comments =
    let
        viewComment cmt =
            div []
                [ text cmt.text
                , text (" by: " ++ cmt.author)
                ]
    in
    div []
        [ ulMap viewComment comments
        , Html.form [ onSubmit (Add i) ]
            [ input
                [ placeholder "Add new comment"
                , onInput Input
                , value m.input
                ]
                []
            , input [ type_ "submit" ] []
            ]
        ]



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | lines =
            [ line "def fat(x, y=1):"
            , line "    if x == 0:"
            , line "        return y"
            , line "    else:"
            , line "        return fat(x - 1, x * y)"
            ]
    }
