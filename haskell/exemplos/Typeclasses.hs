module Typeclasses where


data Point a = Pt (a, a) deriving (Show)


instance (Eq a) => Eq (Point a) where
    Pt (x, y) == Pt (x', y') = 
        (x == x') && (y == y')

instance Functor Point where
    fmap f (Pt (x, y)) = Pt (f x, f y)


class Container c where
    nComponents :: c a -> Int
    myFirst :: c a -> Maybe a
    myLast :: c a -> Maybe a
    isEmpty :: c a -> Bool

instance Container Point where
    nComponents pt = 2
    myFirst (Pt (x, y)) = Just x
    myLast (Pt (x, y)) = Just y
    isEmpty pt = False

instance Container Maybe where
    nComponents lst = length lst
    myFirst (Pt (x, y)) = Just x
    myLast (Pt (x, y)) = Just y
    isEmpty pt = False

-- instance Container List where
--     nComponents lst = length lst
--     myFirst (Pt (x, y)) = Just x
--     myLast (Pt (x, y)) = Just y
--     isEmpty pt = False
    
    


