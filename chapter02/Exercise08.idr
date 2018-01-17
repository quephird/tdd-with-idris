module Exercise08

overLength : Nat -> List String -> Nat
overLength n lst =
  length $ filter isLongerThanN lst
  where
    isLongerThanN str = (>) (length str) n
