-- Використання бібліотеки Parsec
{-# OPTIONS_GHC -Wall #-}
module Hp09Parsec where

import Text.ParserCombinators.Parsec 

------ Прості аналізатори  ----
sign :: Parser String 
sign = string "-" <|> pure "" 

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces ; return a}

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

parens :: Parser a -> Parser a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n              
              
number :: Parser Int
number = do s <- sign 
            cs <- many1 digit
            return $ read (s ++ cs) 

-- number = ["-"] digit { digit }.
-- digit = "0" | "1" | ... | "8" | "9".
-- expr = term { addop term }.
-- term = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop = "+" | "-".
-- mulop = "*".

data Expr = Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Lit Int
            deriving Show

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

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n            

exprAll :: Parser Expr 
exprAll = do spaces;
             ex <- expr 
             eof 
             return ex   

runEval :: String -> String 
runEval st = case parse exprAll "" st of 
               Right ex -> show $ eval ex 
               Left  er -> show er

main :: IO()
main = loop 
 
loop :: IO()
loop = do
    putStr ">" 
    input <- getLine
    if null input then return () 
      else do putStrLn (runEval input)  
              loop

