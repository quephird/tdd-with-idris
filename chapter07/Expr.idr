module Expr

import Printf

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abz (Expr num)

Num num => Num (Expr num) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg num => Neg (Expr num) where
    negate x = 0 - x
    (-) = Sub

Abs num => Abs (Expr num) where
    abs = Abz . abs

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abz x) = abs (eval x)

-- Exercise 7.2.1

Show num => Show (Expr num) where
    show (Val e)     = show e
    show (Add e1 e2) = printf "(%s + %s)" (show e1) (show e2)
    show (Sub e1 e2) = printf "(%s - %s)" (show e1) (show e2)
    show (Mul e1 e2) = printf "(%s * %s)" (show e1) (show e2)
    show (Div e1 e2) = printf "(%s / %s)" (show e1) (show e2)
    show (Abz e)     = printf "|%s|" (show e)

-- Exercise 7.2.2

(Eq num, Abs num, Integral num, Neg num) => Eq (Expr num) where
  (==) x y = (eval x) == (eval y)

-- Exercise 7.2.3

(Integral num, Abs num, Eq num, Neg num) => Cast (Expr num) num where
    cast = eval
