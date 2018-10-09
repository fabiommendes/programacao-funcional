module Pages.Eval360 exposing (Model, Msg, init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (splitAt, updateAt)
import Maybe exposing (withDefault)
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


type Feedback
    = Negative
    | Neutral
    | Positive
    | Extraordinary


type alias Model =
    { users : List User
    , questions : List Question
    }


type alias Question =
    { text : String
    , id : Int
    }


type alias User =
    { name : String
    , userId : Int
    , overall : Feedback
    , questions : Dict Int Feedback
    }


init : Model
init =
    { users = [], questions = [] }


mkUser : String -> Int -> User
mkUser name id =
    User name id Neutral Dict.empty


mkQuestion : String -> Int -> Question
mkQuestion =
    Question


feedbackRepr : Feedback -> String
feedbackRepr fb =
    case fb of
        Negative ->
            "-1"

        Neutral ->
            "0"

        Positive ->
            "+1"

        Extraordinary ->
            "+2"


rank : Feedback -> Int -> User -> User
rank fb id user =
    { user | questions = Dict.insert id fb user.questions }


swapAt i lst =
    let
        ( start, end ) =
            splitAt i lst
    in
    case end of
        [] ->
            start

        [ x ] ->
            lst

        x :: y :: tail ->
            start ++ (y :: x :: tail)



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Rank Int Feedback
    | Evaluate Int Int Feedback
    | Upgrade Int
    | Downgrade Int


update : Msg -> Model -> Model
update msg m =
    case msg of
        Upgrade i ->
            if i /= 0 then
                { m | users = swapAt (i - 1) m.users }

            else
                m

        Downgrade i ->
            { m | users = swapAt i m.users }

        Evaluate i qid fb ->
            { m | users = updateAt i (rank fb qid) m.users }

        _ ->
            m



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "360 User" ]
        , ulIndexedMap (viewUser m) m.users
        ]


viewUser : Model -> Int -> User -> Html Msg
viewUser m i ev =
    let
        toggle st msg =
            span [ style "padding" "5px", onClick (msg i) ] [ text st ]
    in
    div []
        [ h2 [] [ text ev.name, toggle "^" Upgrade, toggle "v" Downgrade ]
        , h3 [] [ text "Feedback" ]
        , viewEvaluations m i ev.questions
        ]


viewEvaluations : Model -> Int -> Dict Int Feedback -> Html Msg
viewEvaluations m i dic =
    ul [] (List.map (viewEvaluation i dic) m.questions)


viewEvaluation : Int -> Dict Int Feedback -> Question -> Html Msg
viewEvaluation i dic qst =
    let
        padding =
            style "padding" "5px"

        userEval =
            Dict.get qst.id dic |> withDefault Neutral

        thumbs label fb =
            div [ padding, onClick (Evaluate i qst.id fb) ] [ text label ]
    in
    li [ style "display" "flex" ]
        [ span [ padding ] [ text (feedbackRepr userEval) ]
        , div [ style "padding" "5px" ] [ text qst.text ]
        , thumbs "-1" Negative
        , thumbs "0" Neutral
        , thumbs "+1" Positive
        , thumbs "+2" Extraordinary
        ]



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | users =
            [ mkUser "Linus" 1
            , mkUser "Guido" 42
            , mkUser "Evan" 120
            ]
        , questions =
            [ mkQuestion "Initiative" 0
            , mkQuestion "Hard work" 1
            , mkQuestion "Knowledge" 2
            ]
    }
