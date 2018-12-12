module Main where

data Result a = 
    Fail | Value String a deriving (Eq, Show)

type Expr a = (String -> Result a)


instance Functor Result where
    fmap f Fail = Fail 
    fmap f (Value tks x) = Value tks (f x)


--- MAIN 0----------------------------------------------------------------------
main :: IO ()
main = do
    src <- getLine
    print (calc src)


--- PARSER ---------------------------------------------------------------------
calc :: String -> Float
calc st = parse expr (filter (/= ' ') st)


parse :: Expr Float -> String -> Float
parse expr st = 
    case expr st of
        Value _ x -> x
        Fail -> 0
    

--- LEXER ----------------------------------------------------------------------
isDigit :: Char -> Bool
isDigit x = x >= '0' && x <= '9'  


--- GRAMMAR --------------------------------------------------------------------
expr :: Expr Float
expr = oneof 
    [ binOp (+) term (chr '+') expr
    , binOp (-) term (chr '-') expr
    , single term
    ]


term :: Expr Float
term = oneof
    [ binOp (*) atom (chr '*') term
    , binOp (/) atom (chr '/') term
    , single atom
    ]

        
atom :: Expr Float
atom = oneof
    [ single number
    , nth 1 [chr '(', expr, chr ')']
    ]


number :: Expr Float
number "" = Fail
number st = Value rest (read nums) 
    where
        nums = takeWhile isDigit st
        rest = dropWhile isDigit st



--- GENERIC PARSER COMBINATOR FUNCTIONS ----------------------------------------
oneof :: [Expr a] -> Expr a
oneof [] _ = Fail
oneof [e] chr = e chr
oneof (e:es) tks = 
    case e tks of
        Fail -> oneof es tks
        res  -> res


chain :: [Expr a] -> Expr [a]
chain [] _ = Fail
chain [e] tks = fmap (:[]) $ e tks
chain (e:es) tks =
    case e tks of
        Value tks' a -> (a:) <$> chain es tks'
        Fail         -> Fail


nth :: Int -> [Expr a] -> Expr a
nth i es tks = 
    case chain es tks of
        Value tks xs -> Value tks (xs !! i)
        Fail         -> Fail


binOp :: (a -> a -> a) -> Expr a -> Expr a -> Expr a -> Expr a
binOp op e1 e2 e3 tks =
    case chain [e1, e2, e3] tks of
        Value tks' [x, _, y] -> Value tks' (op x y)
        _                    -> Fail 


chr :: Char -> Expr Float
chr chr (chr':tks)
    | chr == chr' = Value tks 0
    | otherwise = Fail
chr _ _ = Fail


single x = x
