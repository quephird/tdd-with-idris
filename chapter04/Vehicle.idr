module Vehicle

data PowerSource = Petrol | Feet

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Feet
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200
