{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.List    
data Result a = Fail | Success String a
    deriving (Eq, Show, Functor, Foldable, Traversable)

data Parser a = Parser String (String -> Result a) 
    deriving (Functor)

data Chain b a = Chain [b] [a] 
    deriving (Eq, Show, Functor, Foldable, Traversable)

(=/=) = (/=) -- porque pode :)
singleton = (:[])


--- PROGRAMA PRINCIPAL

main :: IO ()
main = do
    src <- getLine
    print $ parse (filter (=/= ' ') src)
    

parse :: String -> Maybe Double
parse src = run expr st


run :: Parser a -> String -> Maybe a
run (Parser st f) = f st


--- GRAMATICA

expr :: Parser Double
expr = oneOf 
    [ calc <$> chainExpr
    , atom 
    ]
    
chainExpr :: Parser (Chain Char Double)
chainExpr = oneOf 
    [ chain atom op chainExpr
    , End <$> atom st
    ]

atom :: Parser Double
atom = oneOf
    [ float
    , middle (symb '(') expr (symb ')') 
    ]

op :: Parser Char
op "" = Fail
op (c:st) = 
    if elem c "+-*/" 
        then Success st c 
        else Fail
        
chain :: Parser a -> Parser b -> Parser (Chain b a) -> Parser (Chain b a)
chain px pOp py st = do
    case px st of
        Fail -> Fail
        Success st1 x ->
            case pOp st1 of 
                Fail -> Fail
                Success st2 op ->
                    case py st2 of
                        Fail -> Fail
                        Success st3 y -> Success st3 (Elem x op y)

--- BASIC PARSERS
setString :: String -> Parser a -> Parser a
setString st (Parser _ f) = Parser st f

setFunc :: (String -> Maybe b) -> Parser a -> Parser b
setFunc f (Parser st _) = Parser st f

failP :: String -> Parser a
failP st = Parser st (\_ -> Nothing)

success :: String -> a -> Parser a
success st x = Parser st (\_ -> x)

char :: a -> Char -> Parser a
char = setFunc . char'
char' x c "" = Fail
char' x c (c':cs)
    | c == c'   = Success cs x
    | otherwise = Fail

symb :: Char -> Parser ()
symb = char ()

float :: Parser Double
float st = 
    case num of
        [] -> Fail
        _  -> Success rest (read num) 
    where
        rest = dropWhile isDigit st
        num = takeWhile isDigit st
        isDigit c = c >= '0' && c <= '9'  


--- PARSER COMBINATORS

sequential :: [Parser a] -> Parser [a]
sequential [] st = Success st []
sequential [p] st = singleton <$> p st
sequential (p:ps) st =
    case p st of
        Success st' x -> (x:) <$> sequential ps st'
        Fail -> Fail


binOp :: (a -> b -> c) -> Parser a -> Parser op -> Parser b -> Parser c
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


middle :: Parser a -> Parser b -> Parser c -> Parser b
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


oneOf :: [Parser a] -> Parser a
oneOf [] st     = Fail 
oneOf [p] st    = p st
oneOf (p:ps) st =
    case p st of 
        Fail -> oneOf ps st
        succ -> succ  


data Chain op a = End a | Elem a op (Chain op a) 
    deriving (Eq, Show, Functor, Foldable, Traversable, Ord)

data Tree op a = Leaf a | Node (Tree op a) op (Tree op a) 
    deriving (Eq, Functor, Foldable, Traversable, Ord)

instance (Show a, Show b) => Show (Tree a b) where
    show (Leaf a) = show a
    show (Node x op y) = "(" <> content <> ")" 
        where content = intercalate " " [show op, show x, show y]

class CoFunctor cf where 
    coMap :: (f -> g) -> cf f a -> cf g a

(<$$>) :: (CoFunctor cf) => (a -> b) -> cf a c -> cf b c
(<$$>) = coMap

infixr 4 <$$>


foldOp :: Tree (a -> a -> a) a -> a
foldOp (Leaf x)      = x
foldOp (Node x op y) = op (foldOp x) (foldOp y)


tree :: Chain Char Double -> Tree (Double -> Double -> Double) Double
tree (End x) = Leaf x
tree chain = makeOp $ flatten $ treeL (treeL <$> chainPrec "+-" chain)

calc ch = foldOp $ tree ch

treeL :: Chain op a -> Tree op a
treeL (End x) = Leaf x
treeL (Elem x op tail) = tree (Leaf x) op tail
    where
        tree acc op (End y) = Node acc op (Leaf y)
        tree acc op (Elem y op' rhs) = tree lhs op' rhs
            where 
                lhs = Node acc op (Leaf y)


treeR :: Chain op a -> Tree op a
treeR (End x) = Leaf x
treeR (Elem x op tail) = Node (Leaf x) op (treeR tail)


flatten :: Tree a (Tree a b) -> Tree a b 
flatten (Leaf t) = t
flatten (Node x op y) = Node (flatten x) op (flatten y)

chainPrec :: (Eq a) => [a] -> Chain a b -> Chain a (Chain a b)
chainPrec ops (End x) = End (End x)
chainPrec ops (Elem x op tail) = mirror <$> split (End x) op tail
    where 
        split acc op (End x)
            | elem op ops = Elem (acc) op (End (End x))
            | otherwise   = End (Elem x op acc)
        split acc op (Elem x op' y)
            | elem op ops = Elem (mirror acc) op (split (End x) op' y)
            | otherwise   = split (Elem x op acc) op' y


mirror :: Chain a b -> Chain a b
mirror (End x) = End x
mirror (Elem x op xs) = run xs op (End x)
    where
        run (End y) op x = Elem y op x
        run (Elem y op' z) op x = run z op' (Elem y op x) 


isEnd :: Chain op a -> Bool
isEnd (End _) = True
isEnd _       = False

makeOp :: (Fractional a, CoFunctor cf) => cf Char a -> cf (a -> a -> a) a
makeOp = (toOp <$$>)
    where
        toOp '+' = (+)
        toOp '-' = (-)
        toOp '*' = (*)
        toOp '/' = (/)

instance CoFunctor Tree where
    coMap f (Leaf x) = Leaf x
    coMap f (Node x op y) = Node (f <$$> x) (f op) (f <$$> y)

pn :: [a] -> [b] -> Chain b a
pn (x:xs) (op:ops) = Elem x op (pn xs ops)
pn [x] _ = End x
