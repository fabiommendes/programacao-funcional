module Teste exposing (fat, fat1, fatAcc, myMap)


fat1 n =
    if n <= 1 then
        1

    else
        n * fat1 (n - 1)


fatAcc n acc =
    if n <= 1 then
        acc

    else
        fatAcc (n - 1) (n * acc)


myMap : (a -> b) -> List a -> List b
myMap func lst =
    case lst of
        [] ->
            []

        x :: tail ->
            func x :: myMap func tail


fat n =
    fatAcc n 1


fatList : List number
fatList =
    myMap fat [ 1, 2, 3, 5 ]
