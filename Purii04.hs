{-# OPTIONS_GHC -Wall #-}
module Purii04 where

data Expression = Var String                  -- Змінна
               | Val Int                      -- Ціла константа
               | Op Expression Bop Expression -- Операція
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Divide
          | Gt | Ge | Lt | Le| Eql
            deriving (Show, Eq)

data Statement = Assign String Expression
               | Incr String
               | If Expression Statement Statement
               | While Expression Statement
               | For Statement Expression Statement Statement
               | Sequence Statement Statement
               | Skip
                 deriving (Show, Eq)

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

type State = [(String, Int)]

-- Задача 1 -----------------------------------------
get ::  State -> String -> Int
get [] _ = 0
get ((lkey, lvalue) : ltail) key
    | lkey == key    = lvalue
    | otherwise      = get ltail key

-- Задача 2 -----------------------------------------
extend :: State -> String -> Int -> State
extend [] key value = ((key, value) : [])
extend ((lkey, lvalue) : ltail) key value
    | lkey == key    = ((lkey, value) : ltail)
    | otherwise      = ((lkey, lvalue) : (extend ltail key value))

-- Задача 3 -----------------------------------------
evalE :: State -> Expression -> Int
evalE st (Var x) = get st x
evalE _ (Val x) = x
evalE st (Op expr1 bop expr2) = case bop of
    Plus   -> arg1 + arg2
    Minus  -> arg1 - arg2
    Times  -> arg1 * arg2
    Divide -> arg1 `div` arg2
    Gt     -> toInt (arg1 > arg2)
    Ge     -> toInt (arg1 >= arg2)
    Le     -> toInt (arg1 <= arg2)
    Lt     -> toInt (arg1 < arg2)
    Eql    -> toInt (arg1 == arg2)
    where
        arg1 = evalE st expr1
        arg2 = evalE st expr2

        toInt True = 1
        toInt False = 0

-- Задача 4 -----------------------------------------
desugar :: Statement -> DietStatement
desugar (Assign key expr) = DAssign key expr
desugar (Incr key) = DAssign key (Op (Var key) Plus (Val 1))
desugar (If expr stm1 stm2) = DIf expr (desugar stm1) (desugar stm2)
desugar (While expr stm) = DWhile expr (desugar stm)
desugar (For stmi expr stmn stm) = DSequence (desugar stmi) (DWhile expr (DSequence (desugar stm) (desugar stmn)))
desugar (Sequence stm1 stm2) = DSequence (desugar stm1) (desugar stm2)
desugar (Skip) = DSkip

-- Задача 5 -----------------------------------------
evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign key expr) = extend st key (evalE st expr)
evalSimple st (DIf expr dstm1 dstm2) =
    if (evalE st expr) == 1
        then evalSimple st dstm1
        else evalSimple st dstm2
evalSimple st (DWhile expr dstm) =
    if (evalE st expr) == 1
        then evalSimple (evalSimple st dstm) (DWhile expr dstm)
        else st
evalSimple st (DSequence dstm1 dstm2) = evalSimple (evalSimple st dstm1) dstm2
evalSimple st (DSkip) = st

-- Задача 6 -----------------------------------------
run :: State -> Statement -> State
run st stm = evalSimple st (desugar stm)

-- Програми -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Обчислення значення b(6) в степені e(5) в змінній out 

   { b := 6; e := 5; out:= 1;
     for (i:=0; i<e; i++) out := out*b   
   }
-}

power :: Statement
power = slist [ Assign "b" (Val 6)
              , Assign "e" (Val 5) 
              , Assign "out" (Val 1)
              , For (Assign "i" (Val 0))
                    (Op (Var "i") Lt (Var "e"))
                    (Incr "i")
                    (Assign "out" (Op (Var "out") Times (Var "b")))
              ] 

{- Обчислення цілого значення корня квадратного 
   зі значення змінної a (317) в змінній b 

   {a := 317; b := 0;
    while (a >= b*b) b++;
    b := b-1
   } 	
-}
squareRoot :: Statement
squareRoot = slist [ Assign "a" (Val 317)
                   , Assign "b" (Val 0)
                   , While (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                       (Incr "b")
                   , Assign "b" (Op (Var "b") Minus (Val 1))
                   ]

{- Обчислює значення 12-го числа Фібонначі в змінній out

  {in := 12; f0 := 1; f1 := 1;
   if (in == 0) then out := f0 else 
     if (in == 1) then out := f1 else 
       for (c := 2; c < in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
       }
  }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "in" (Val 12)
                  , Assign "f0" (Val 1)
                  , Assign "f1" (Val 1)
                  , If (Op (Var "in") Eql (Val 0))
                       (Assign "out" (Var "f0"))
                       (If (Op (Var "in") Eql (Val 1))
                           (Assign "out" (Var "f1"))
                           (For (Assign "c" (Val 2))
                                (Op (Var "c") Lt (Var "in"))
                                (Incr "c")
                                (slist
                                 [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                 , Assign "f0" (Var "f1")
                                 , Assign "f1" (Var "out")
                                 ])
                           )
                       )
                  ]
