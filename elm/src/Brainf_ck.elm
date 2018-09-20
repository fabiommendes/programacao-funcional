module Brainf_ck exposing
    ( Tape
    , change
    , decr
    , down
    , emptyTape
    , incr
    , main
    , map
    , moveLeft
    , moveRight
    , runBf
    , toList
    , up
    )

import Debug
import Html exposing (..)


type alias Tape a =
    { left : List a
    , right : List a
    }


emptyTape : Tape Int
emptyTape =
    Tape [] [ 0 ]


up : Int -> Int
up x =
    modBy 256 (x + 1)


down : Int -> Int
down x =
    modBy 256 (x - 1)


incr : Tape Int -> Tape Int
incr =
    change up 0


decr : Tape Int -> Tape Int
decr =
    change down 0


map : (a -> b) -> Tape a -> Tape b
map f { left, right } =
    Tape (List.map f left) (List.map f right)


getChar : Tape Int -> Char
getChar { right } =
    case right of
        [] ->
            Char.fromCode 0

        x :: _ ->
            Char.fromCode x


setItem : a -> Tape a -> Tape a
setItem x { left, right } =
    case right of
        [] ->
            Tape left [ x ]

        _ :: tail ->
            Tape left (x :: tail)


change : (a -> a) -> a -> Tape a -> Tape a
change f default { left, right } =
    case right of
        [] ->
            Tape left [ f default ]

        head :: tail ->
            Tape left (f head :: tail)


moveLeft : Tape Int -> Tape Int
moveLeft { left, right } =
    case left of
        head :: tail ->
            Tape tail (head :: right)

        [] ->
            Tape [] (0 :: right)


moveRight : Tape Int -> Tape Int
moveRight { left, right } =
    case right of
        head :: tail ->
            Tape (head :: left) tail

        [] ->
            Tape (0 :: left) [ 0 ]


toList : Tape a -> List a
toList { left, right } =
    List.reverse left ++ right


runBf : String -> String -> Tape Int -> ( Tape Int, String )
runBf code inputs tape =
    let
        runChars chars inputList out t =
            case chars of
                [] ->
                    ( t, String.fromList <| List.reverse out )

                '<' :: tail ->
                    runChars tail inputList out (moveLeft t)

                '>' :: tail ->
                    runChars tail inputList out (moveRight t)

                '+' :: tail ->
                    runChars tail inputList out (incr t)

                '-' :: tail ->
                    runChars tail inputList out (decr t)

                '.' :: tail ->
                    runChars tail inputList (getChar t :: out) t

                ',' :: tail ->
                    case inputList of
                        [] ->
                            runChars tail tail out (setItem 0 t)

                        head :: inputTail ->
                            let
                                value =
                                    Char.toCode head
                            in
                            runChars tail inputTail out (setItem value t)

                _ :: tail ->
                    runChars tail inputList out t
    in
    runChars
        (String.toList code)
        (String.toList inputs)
        []
        tape


main =
    text (Debug.toString emptyTape)
