{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import Data.List

data Result a = Fail | Success String a deriving (Eq, Show, Functor)
type Runner a = (String -> Result a)
data Parser a = Parser String (Runner a)

(=/=) = (/=) -- porque pode :)
singleton = (:[])

instance Functor Parser where
    fmap f (Parser st g) = Parser st (\x -> f <$> g x)

-- instance Applicative Parser where
--     Parser st0 g <*> Parser st1 f = 
--         case f st1 of
--             Fail -> Parser st0 (\_ -> Fail)
--             Success st2 x -> 
--                 case g st0 of
--                     Fail -> Parser st0 (\_ -> Fail)
--                     Success st3 h -> Parser st3 (h x)

    pure x = Parser "" (\_ -> Success "" x)
    
instance Applicative Result where
    Success st f <*> Success _ x = Success st (f x)
    x <*> y = Fail
    pure x = Success "" x

instance Monad Result where
    Fail >>= f = Fail
    Success st a >>= f = 
        case f a of
            Fail -> Fail
            Success st' b -> Success st b

--- PROGRAMA PRINCIPAL

main :: IO ()
main = do
    src <- getLine
    print $ parse (filter (=/= ' ') src)
    

parse :: String -> Result Double
parse src = expr src


--- GRAMATICA

expr :: Runner Double
expr = oneOf 
    [ binOp (+) term (symb '+') expr
    , binOp (-) term (symb '-') expr
    , atom 
    ]

term :: Runner Double
term = oneOf 
    [ binOp (*) atom (symb '*') term
    , binOp (/) atom (symb '/') term
    , atom 
    ]
    
atom :: Runner Double
atom = oneOf
    [ float
    , middle (symb '(') expr (symb ')') 
    ]


--- BASIC PARSERS

char :: a -> Char -> Runner a
char x c "" = Fail
char x c (c':cs)
    | c == c'   = Success cs x
    | otherwise = Fail

symb :: Char -> Runner ()
symb = char () 

float :: Runner Double
float st = 
    case num of
        [] -> Fail
        _  -> Success rest (read num) 
    where
        rest = dropWhile isDigit st
        num = takeWhile isDigit st
        isDigit c = c >= '0' && c <= '9'  


--- PARSER COMBINATORS

sequential :: [Runner a] -> Runner [a]
sequential [] st = Success st []
sequential [p] st = singleton <$> p st
sequential (p:ps) st =
    case p st of
        Success st' x -> (x:) <$> sequential ps st'
        Fail -> Fail


binOp :: (a -> b -> c) -> Runner a -> Runner op -> Runner b -> Runner c
binOp f px op py st = 
    case px st of
        Fail -> Fail 
        Success st1 x -> 
            case op st1 of
                Fail -> Fail 
                Success st2 _ -> 
                    case py st2 of
                        Fail -> Fail 
                        Success st3 y -> Success st3 (f x y)


middle :: Runner a -> Runner b -> Runner c -> Runner b
middle px py pz st = 
    case px st of
        Fail -> Fail 
        Success st1 _ -> 
            case py st1 of
                Fail -> Fail 
                Success st2 middle -> 
                    case pz st2 of
                        Fail -> Fail 
                        Success st3 _ -> Success st3 middle


oneOf :: [Runner a] -> Runner a
oneOf [] st     = Fail 
oneOf [p] st    = p st
oneOf (p:ps) st =
    case p st of 
        Fail -> oneOf ps st
        succ -> succ  
