module Physics where

data Comida a = Burrito a | Tapioca a deriving (Eq, Show)

instance Functor Comida where
    fmap f (Burrito x) = Burrito (f x)
    fmap f (Tapioca x) = Tapioca (f x)

instance Applicative Comida where
    pure a = Tapioca a
    Burrito f <*> c = Burrito (f $ unwrap c)   
    Tapioca f <*> c = Tapioca (f $ unwrap c)

instance Monad Comida where
    return = pure
    Burrito x >>= f = Burrito(unwrap $ f x)
    Tapioca x >>= f = Tapioca(unwrap $ f x)    
    
bc = Burrito "carne"
bq = Burrito "queso"
tc = Tapioca "carne"
tq = Tapioca "queijo"

pepper = (++"!") 

unwrap (Burrito x) = x
unwrap (Tapioca x) = x 

join3 a b c = a ++ b ++ c

criatapioca = do
    x <- bq
    y <- bc
    return (pepper x ++ " " ++ pepper y)

troca_recheio = do
    tq
    bc