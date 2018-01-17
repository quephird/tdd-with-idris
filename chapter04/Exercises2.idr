module Exercises2

import Data.Vect

-- Exercises 1 and 2

data PowerSource = Petrol | Feet | Electric

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Feet
  Bicycle : Vehicle Feet
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : Vehicle Electric
  Tram : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4
wheels ElectricCar = 4
wheels Tram = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle _) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

-- Exercises 3 and 4

takeVect : (m : Fin n) -> Vect n a -> Vect (finToNat m) a
takeVect FZ xs = []
takeVect (FS m') (x :: xs) = x :: takeVect m' xs

-- Exercise 5

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                         Nothing => Nothing
                         Just idx => Just (index idx xs)

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing => Nothing
    (Just pos') => Just $ (index pos' xs) + (index pos' ys)
