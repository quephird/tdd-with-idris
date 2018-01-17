module Exercises

import Picture
import Shape
import Tree

-- Exercise 1

listToTree : Ord elem => List elem -> Tree elem
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

-- Exercise 2

treeToList : Ord elem => Tree elem -> List elem
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

-- Exercise 3

data Expr = Value Int
          | Sum Expr Expr
          | Difference Expr Expr
          | Product Expr Expr

-- Exercise 4

evaluate : Expr -> Int
evaluate (Value val) = val
evaluate (Sum x y) = evaluate x + evaluate y
evaluate (Difference x y) = evaluate x - evaluate y
evaluate (Product x y) = evaluate x * evaluate y

-- Exercise 5

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing right = right
maxMaybe left Nothing = left
maxMaybe left@(Just x) right@(Just y) =
  case compare x y of
    LT => right
    EQ => right
    GT => left

-- Exercise 6

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) =
  case shape of
    (Triangle _ _) => Just $ area shape
    (Rectangle _ _) => Nothing
    (Circle _) => Nothing
biggestTriangle (Combine pict1 pict2) = maxMaybe (biggestTriangle pict1) (biggestTriangle pict2)
biggestTriangle (Rotate x pict) = biggestTriangle pict
biggestTriangle (Translate x y pict) = biggestTriangle pict
