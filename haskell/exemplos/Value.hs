module Value where

data Value a = Value a deriving (Show, Eq)

instance Functor Value where
    fmap f (Value x) = Value (f x)

instance Applicative Value where
    pure = Value
    Value f <*> Value x = Value (f x)

instance Monad Value where
    Value x >>= f = f x
    return = Value



data Zip a = Zip [a] deriving (Eq, Show)

instance Functor Zip where
    fmap f (Zip lst) = Zip (fmap f lst)

instance Applicative Zip where
    pure x = Zip (repeat x)
    Zip fs <*> Zip xs = Zip (zipWith (\f x -> f x) fs xs)

instance Monad Zip where
    return = pure
    Zip xs >>= f  = Zip $ bind xs (fmap unwrap f) where
        bind [] f     = []
        bind (x:xs) f = 
            case f x of
                [] -> []
                y:_ -> y : bind xs (fmap tailOrNil f)

        tailOrNil []     = []
        tailOrNil (x:xs) = xs


unwrap (Zip xs) = xs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten(xs)

res = do
    a <- Zip [1, 2, 3]
    b <- Zip [3, 2, 1]
    return (a * b)