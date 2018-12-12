module Main where

data Token = 
    LPar | RPar | Add | Sub | Mul | Div | Number Float deriving (Eq, Show)

data Result a = 
    Fail | Value [Token] a deriving (Eq, Show)

type Expr a = ([Token] -> Result a)


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
calc st = parse expr (tokenize st)


parse :: Expr Float -> [Token] -> Float
parse expr tks = 
    case expr tks of
        Value [] x -> x
  

--- LEXER ----------------------------------------------------------------------
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:st)
    | c == ' '   = tokenize st
    | c == '('   = LPar : rest
    | c == ')'   = RPar : rest
    | c == '+'   = Add : rest
    | c == '-'   = Sub : rest
    | c == '*'   = Mul : rest
    | c == '/'   = Div : rest
    | isDigit c  = Number n : (tokenize $ dropWhile isDigit st)
    where 
        n = read $ c : takeWhile isDigit st
        rest = tokenize st


isDigit :: Char -> Bool
isDigit x = x >= '0' && x <= '9'  


number :: Token -> Maybe Float
number (Number x) = Just x
number _          = Nothing


--- GRAMMAR --------------------------------------------------------------------
expr :: Expr Float
expr = oneof 
        [ binOp (+) term (tk Add) expr
        , binOp (-) term (tk Sub) expr
        , single term
        ]


term :: Expr Float
term = oneof
     [ binOp (*) atom (tk Mul) term
     , binOp (/) atom (tk Div) term
     , single atom
     ]

     
atom :: Expr Float
atom = oneof
     [ match number
     , nth 1 [tk LPar, expr, tk RPar]
     ]


--- GENERIC PARSER COMBINATOR FUNCTIONS ----------------------------------------
oneof :: [Expr a] -> Expr a
oneof [] _ = Fail
oneof [e] tk = e tk
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


tk :: Token -> Expr Float
tk tk (tk':tks)
    | tk == tk' = Value tks 0
    | otherwise = Fail
tk _ _ = Fail


match :: (Token -> Maybe a) -> Expr a
match isToken (tk:tks) =
    case isToken tk of
        Just n -> Value tks n
        _      -> Fail 
match _ _ = Fail


single x = x
