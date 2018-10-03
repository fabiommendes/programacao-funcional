module MyList where

type Size = Int
type Value = Int


-- Para casa:: descomenta a linha e segue o 
-- compilador até funcionar
data MyList = Cons Value Size MyList | Nil deriving (Show)


nil :: MyList
nil = Nil

singleton :: Value -> MyList
singleton x = Cons x 1 nil

cons :: Value -> MyList -> MyList
cons x lst = Cons x (1 + myLength lst) lst

babyMap :: (Value -> Value) -> MyList -> MyList
babyMap f Nil = Nil
babyMap f (Cons head size tail) = cons newHead newTail 
    where
        newHead = f head
        newTail = babyMap f tail
            
myMap :: (Value -> Value) -> MyList -> MyList
myMap f lst = myReverse $ mapAux f lst Nil

mapAux :: (Value -> Value) -> MyList -> MyList -> MyList
mapAux f lst acc = 
    case lst of 
        Nil -> acc
        Cons head size tail ->
            let
                newHead = f head
                newAcc = cons newHead acc 
            in 
            mapAux f tail newAcc


myReverse :: MyList -> MyList
myReverse lst = 
    mapAux id lst nil


myLength :: MyList -> Int
myLength lst =
    case lst of 
        Nil -> 0
        Cons x size tail -> 
            size

join :: MyList -> MyList -> MyList
join first second =
    let
        joinRev xs ys =
            case xs of
                Nil -> ys
                Cons head size tail -> 
                    joinRev tail (cons head ys)
    in
    joinRev (myReverse first) second



example =
    cons 1 (cons 2 (singleton 3))
