{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import Data.List

data Result a = Fail | Success String a deriving (Eq, Show, Functor)
type Runner a = (String -> Result a)
data Parser a = Parser String (Runner a)
data Chain a  = Chain [Char] [a] deriving (Eq, Show, Functor)

(=/=) = (/=) -- porque pode :)
infix 4 =/= -- mesma precedencia do /=
singleton = (:[])
(??) = (\_->()) <$> ((:[(,,)]) (,,))

-- CONVERTER PARA HASKELL
{-

((~({}+[])+~({}+[]))*(~({}+[])+~({}+[]))*(~({}+[])+~({}+[]))
*(~({}+[])+~({}+[])))+((~({}+[])+~({}+[]))*(~({}+[])
+~({}+[]))*(~({}+[])+~({}+[]))*(~({}+[])+~({}+[])))
+((~({}+[])+~({}+[]))*(~({}+[])+~({}+[])))+((~({}+[])
+~({}+[]))*(~({}+[])+~({}+[])))+(~({}+[])*~({}+[]))
+(~({}+[])*~({}+[]))

-}

instance Functor Parser where
    fmap f (Parser st g) = Parser st (\x -> f <$> g x)
    
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
    print $ parse src

parse :: String -> Result Double
parse src = expr (filter (=/= ' ') src)


--- GRAMATICA

-- expr :: Runner Double
-- expr = oneOf 
--     [ binOp (+) term (symb '+') expr
--     , binOp (-) term (symb '-') expr
--     , term 
--     ]

-- term :: Runner Double
-- term = oneOf 
--     [ binOp (*) atom (symb '*') term
--     , binOp (/) atom (symb '/') term
--     , atom 
--     ]

expr :: Runner Double
expr = \st -> calc <$> (opChain st)


opChain :: Runner (Chain Double)
opChain = oneOf
    [ op3 mkChain atom op opChain
    , mkSingle float 
    ]

op :: Runner Char
op "" = Fail
op (c:cs)
    | c `elem` "+-*/" = Success cs c
    | otherwise       = Fail

atom :: Runner Double
atom = oneOf
    [ float
    , middle (symb '(') expr (symb ')') 
    ]

-- IMPLEMENTAR DIREITO!!!
calc :: Chain Double -> Double
calc x = calcPlus (calcMul x)

calcMul :: Chain Double -> Chain Double
calcMul (Chain [] nums) = Chain [] nums
calcMul (Chain (op:ops) (x:y:xs)) 
    | op == '*' = calcMul $ Chain ops ((x * y):xs)
    | op == '/' = calcMul $ Chain ops ((x / y):xs)
    | otherwise = Chain (op:ops') (x:xs')
    where 
        Chain ops' xs' = calcMul $ Chain ops (y:xs)

        
calcPlus :: Chain Double -> Double
calcPlus (Chain [] (x:xs)) = x 
calcPlus (Chain ('+':ops) (x:y:xs)) = calcPlus $ Chain ops ((x+y):xs)
calcPlus (Chain ('-':ops) (x:y:xs)) = calcPlus $ Chain ops ((x-y):xs)


mkChain :: Double -> Char -> Chain Double -> Chain Double
mkChain n op (Chain ops nums) = undefined 


mkSingle :: Runner a -> Runner (Chain a)
mkSingle p =  \st -> simpleChain <$> p st

simpleChain :: a -> Chain a
simpleChain x = Chain [] [x]


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


op3 :: (a -> b -> c -> d) -> Runner a -> Runner b -> Runner c -> Runner d
op3 f px py pz st = 
    case px st of
        Fail -> Fail 
        Success st1 x -> 
            case py st1 of
                Fail -> Fail 
                Success st2 y -> 
                    case pz st2 of
                        Fail -> Fail 
                        Success st3 z -> Success st3 (f x y z)


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
