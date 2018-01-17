module Exercises

import Data.Vect

-- Exercise 0

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

myVectMap : (a -> b) -> Vect n a -> Vect n b
myVectMap f [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs

-- Exercise 1

createEmpties : Num numType => Vect n (Vect 0 numType)
createEmpties = replicate _ []

myTransposeHelper : Num numType => (x : Vect n numType)
                    -> (transposedXs : Vect n (Vect len numType))
                    -> Vect n (Vect (S len) numType)
myTransposeHelper x y = zipWith (::) x y

myTranspose : Num numType => Vect m (Vect n numType)
              -> Vect n (Vect m numType)
myTranspose [] = createEmpties
myTranspose (x :: xs) = let transposedXs = myTranspose xs in
                        myTransposeHelper x transposedXs

-- Exercise 2

add : Num numType => Vect r (Vect c numType)
            -> Vect r (Vect c numType)
            -> Vect r (Vect c numType)
add [] [] = []
add (row1 :: rows1) (row2 :: rows2) =
  (zipWith (+) row1 row2) :: add rows1 rows2

-- Exercise 3

dotProduct : Num numType => Vect m numType
             -> Vect m numType
             -> numType
dotProduct x y = foldl (+) 0 $ zipWith (*) x y

multiplyHelper : Num numType => (x : Vect m (Vect n numType))
                 -> (transposedY : Vect p (Vect n numType))
                 -> Vect m (Vect p numType)
multiplyHelper [] y = []
multiplyHelper (x :: xs) y = map (dotProduct x) y :: multiplyHelper xs y

multiplyMatrix : Num numType => Vect m (Vect n numType)
                 -> Vect n (Vect p numType)
                 -> Vect m (Vect p numType)
multiplyMatrix x y = let transposedY = myTranspose y in
                    multiplyHelper x transposedY
