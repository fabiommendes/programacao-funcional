module MyList exposing
    ( MyList(..)
    , cons
    , example
    , nil
    , singleton
    , map
    , length
    , join
    )

type alias Size = Int
type alias Value = Int


-- Para casa: descomenta a linha e segue o 
-- compilador até funcionar
type MyList
    = Cons Value Size MyList
    | Nil
    -- | Range Value Value


nil : MyList
nil =
    Nil


singleton : Value -> MyList
singleton x =
    Cons x 1 nil


cons : Value -> MyList -> MyList
cons x lst =
    Cons x (1 + length lst) lst


babyMap : (Value -> Value) -> MyList -> MyList
babyMap f lst =
    case lst of
        Nil ->
            Nil

        Cons head size tail ->
            let
                newHead = f head
                newTail = babyMap f tail
            in
            cons newHead newTail


map : (Value -> Value) -> MyList -> MyList
map f lst =
    reverse <| mapAux f lst Nil


mapAux : (Value -> Value) -> MyList -> MyList -> MyList
mapAux f lst acc = 
    case lst of 
        Nil -> acc
        Cons head size tail ->
            let
                newHead = f head
                newAcc = cons newHead acc 
            in 
            mapAux f tail newAcc


reverse : MyList -> MyList
reverse lst = 
    mapAux identity lst nil


length : MyList -> Int
length lst =
    case lst of 
        Nil -> 0
        Cons x size tail -> 
            size

join : MyList -> MyList -> MyList
join first second =
    let
        joinRev xs ys =
            case xs of
                Nil -> ys
                Cons head size tail -> 
                    joinRev tail (cons head ys)
    in
    joinRev (reverse first) second



example =
    cons 1 (cons 2 (singleton 3))
