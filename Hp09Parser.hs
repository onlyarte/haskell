-- Побудова синтаксичних аналізаторів
{-# OPTIONS_GHC -Wall #-}
module Hp09Parser where

import Data.Char

-- Тип для аналізаторів (Parser)
newtype Parser a = Parser { parse :: String -> [(a,String)] }

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)] 
--  parse item "c" .. parse item "nm" .. parse item ""   

-- Приклади аналізаторів
sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> if p c then [(c,cs)]  else [] 

char :: Char -> Parser Char   
char c = sat (==c)  

digit :: Parser Char
digit = sat isDigit  

oneOf :: [Char] -> Parser Char
oneOf s = sat (\c -> elem c s)  -- sat (flip elem s)

------- Класи Functor і Applicative 
-- функтор
-- fmap :: (a -> b) -> Parser a -> Parser b    fmap == <$>   
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) 
  
intDigit :: Parser Int
intDigit = digitToInt <$> digit

-- аплікативний функтор         
-- pure :: a -> Parser a 
-- (<*>) :: Parser a -> Parser (a->b) -> Parser b  
instance Applicative Parser where
  pure a = Parser (\s -> [(a,s)])
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]) 

string :: String -> Parser String 
string "" = pure ""   --- nullSt 
string (c:cs) = (:) <$> (char c) <*> (string cs) 

---- Клас  Monad 
-- return :: a -> Parser a 
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b    
instance Monad Parser where
  return a = Parser (\s -> [(a,s)])
  p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s
         -- Parser (\s -> concatMap (\(a, s') -> parse (f a) s')(parse p s) ) 

-- Детермінований оператор вибору
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> case parse p s of
                           []     -> parse q s
                           res    -> res) 

many  :: Parser a  -> Parser [a]   --  нуль або багато
many p = some p <|> return []     

some  :: Parser a  -> Parser [a]   --  один або багато 						   
some p = do {a <- p; as <- many p; return (a:as)}

number :: Parser Int
number = do s <- sign 
            cs <- some digit
            return $ read (s ++ cs) 

sign :: Parser String 
sign = string "-" <|> return []

-- Прості аналізатори 
spaces :: Parser ()
spaces = many (sat isSpace) >> return ()

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces ; return a}

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

parens :: Parser a -> Parser a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n              

--  Аналізатор послідовності              
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a         

operator :: (Num a) => Parser (a -> a -> a)
operator = inOperator <$> (oneOf "+-*")
  where inOperator :: (Num a) => Char -> (a -> a -> a)
        inOperator c = case c of {'+'-> (+); '-'->(-);'*'->(*); _ -> error "operator"} 

-- Виконання аналізатора		
runParser :: Parser a -> String -> Either String a
runParser m s =
  case parse m s of
    [(res, [])] -> Right res
    [(_, _)]    -> Left "Parser not all stream."
    _           -> Left "Parser error."
 
-- Мова виразів                  
-- number = ["-"] digit { digit }.
-- digit = "0" | "1" | ... | "8" | "9".
-- expr = term { addop term }.
-- term = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop = "+" | "-".
-- mulop = "*".

data Expr = Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Lit Int
                     deriving Show

-- Аналізатор виразів
int :: Parser Expr
int = do { n <-  lexem number; return (Lit n)}

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop, mulop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)
mulop = infixOp "*" Mul

expr, term, factor :: Parser Expr
expr   = term `chainl1` addop
term   = factor `chainl1` mulop
factor = int <|> parens expr

run :: String -> Either String Expr
run = runParser (spaces >> expr)     

-- Калькулятор виразів
eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n            

runEval :: String -> String 
runEval st = case runParser (spaces >> expr) st of 
               Right ex -> show $ eval ex 
               Left  er -> er               

-- Repl (read-eval-print loop)			   
main :: IO()
main = loop 
 
loop :: IO()
loop = do
    putStr ">" 
    input <- getLine
    if null input then return () 
      else do putStrLn (runEval input)  
              loop
 
