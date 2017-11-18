{-# OPTIONS_GHC -Wall #-}
module HW09 where

import Text.ParserCombinators.Parsec
import Data.Char

-- Задача 1 -----------------------------------------
numbers:: String -> Maybe [Double]
numbers str = if (check str) then case (parse (commaSep numbr) "" str) of
 Left _ -> Nothing
 Right xs -> Just xs 
 else Nothing

commaSep :: Parser a -> Parser [a]
commaSep p  = p `sepBy1` (char ';')

numbr :: Parser Double
numbr = do {cs <- many1 (digit<|>(char '.')<|>space); return (read (filter (/=' ') cs))}

check:: String -> Bool
check xs = if null (filter (/=';')(filter (/=' ')(filter (noDigit) (filter (/='.') xs)))) then True else False

noDigit:: Char -> Bool
noDigit c = if (isDigit c) then False else True

-- Задача 2 ----------------------------------------- 


----------------  Мова SPL  ------------   
data Expression =
    Var String                   -- Змінна
  | Val Int                      -- Ціла константа
  | Op Expression Bop Expression -- Операція
  deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop = Plus | Minus | Times | Divide   
         | Gt | Ge | Lt | Le | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)                
-----------------
-- лексика
-----------------
-- number = digit { digit }.
-- iden   = char {digit | char}.   not "if" "while" "for" "else"
-- digit  = "0" | "1" | ... | "8" | "9".
-- addOp  = "+" | "-".
-- mulOp  = "*" | "/".
-- relOp  = "<" | "<=" | ">" | ">=" | "==" 
-- symbol = ';' | '{' | '}' | '(' | ')'
-- keyword = "if" | "while" | "for" | "else"
----------------
identifier :: Parser String
identifier = try( do {name <- iden;
                      if (any(name==) ["for","if","else","while"])
                         then unexpected ("reserved word " ++ show name)
                         else return name 
                     } )          

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Divide)

-- розпізнати всі "порожні" символи в кінці		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Expression -> Bop -> Expression -> Expression 
--   :type flip Op -------> Bop -> Expression -> Expression -> Expression         
exprOp :: Parser Bop -> Parser (Expression -> Expression -> Expression)
exprOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum))  

-- Задача 3 -----------------------------------------
iden :: Parser String
iden = do {spaces;a <- letter; cs <- many (letter<|>digit); return (a:cs)} <?> "iden"

number :: Parser Int
number  = do {cs <- many1 digit; return (read cs)}
 
addOp :: Parser Bop  
addOp = (oper "+" Plus) <|> (oper "-" Minus)

relOp :: Parser Bop  
relOp = try (oper ">=" Ge ) <|>
        try (oper ">"  Gt ) <|>
        try (oper "<=" Le ) <|>
        try (oper "<"  Lt ) <|>
        try (oper "==" Eql)
        <?>
        "relOp"

-----------------
-- вирази
-----------------
-- expr   = simple [relOp simple]
-- simple = term { addOp term }.
-- term   = factor { mulOp factor }.
-- factor = "(" expr ")" | number | identifier .
----------------	
factor :: Parser Expression
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Val nm)}
     <|> do {cs <- lexem identifier; return (Var cs) } 
     <?> "factor" 

-- Задача 4 -----------------------------------------
term :: Parser Expression     
term  = factor `chainl1` (exprOp mulOp) <?> "term" 

simple :: Parser Expression
simple = term `chainl1` (exprOp addOp) <?> "simple" 

expr :: Parser Expression
expr = simple `chainl1` (exprOp relOp) <?> "expr"

-----------------
-- оператори
-----------------
-- stmt  = "for" forSt | "while" whileSt |
--         "if" ifSt | iden assSt | '{' listSt '}' 
-- forSt = '(' stmt ';' expr ';' stmt ')' stmt 
-- whileSt = '(' expr ')' stmt 
-- ifSt  = '(' expr ')' stmt "else" stmt 
-- assSt = "++" | ":=" expr 
-- listSt = [stmt] {';' [stmt]}   
-----------------
stmt :: Parser Statement 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {var <- lexem identifier; assignSt var}
       <|> do {symbol '{'; s <- listSt; symbol '}'; return s }
       <?> "statement"    

stmt1 :: Parser [Statement]
stmt1 = many1 stmt

-- Задача 5 -----------------------------------------
forSt :: Parser Statement 
forSt   = do {spaces; symbol '('; s <- stmt; symbol ';'; e <- expr; symbol ';'; s1 <- stmt; symbol ')'; s2 <- stmt; return (For s e s1 s2)} <?> "forSt"


whileSt :: Parser Statement               
whileSt = do {spaces; symbol '('; e <- expr; symbol ')'; s <- stmt; return (While e s)} <?> "WhileSt"
              
ifSt :: Parser Statement               
ifSt    = do {spaces;symbol '('; e <- expr; symbol ')'; s <- stmt; keyword "else"; s2 <- stmt; return (If e s s2) }
              

listSt :: Parser Statement               
listSt =  do {spaces;s <- stmt `sepBy1` symbol ';';; if (null s) then return (Skip) else return (foldr1 Sequence s)}



assignSt :: String -> Parser Statement 
assignSt var = do {spaces; symbol '+';symbol '+'; return (Incr var)}
                 <|> do {spaces; symbol ':'; symbol '='; e <- expr;  return (Assign var e)}
               
---------------------------------------------	
-- Головні функції
---------------------------------------------				
contents :: Parser a -> Parser a
contents p = do {spaces; r<-p; eof; return r}

parseMain :: String -> Either ParseError Statement 
parseMain s = parse (contents stmt) "" s 

parseFile :: String -> IO (Either ParseError Statement)
parseFile fn = parseFromFile (contents stmt) fn 

---------------------------------------------
--- Дані для тестування
--------------------------------------------- 
power :: String
power =
   "{ b := 6; e := 5; out := 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"

squareRoot :: String
squareRoot =
   "{a := 317; b := 0;\
   \  while (a >= b*b) b++;\
   \  b := b-1\
   \ }"