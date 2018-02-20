occurrences : Eq ty => (item : ty) -> (list : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True  => S $ occurrences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False
  (/=) m1 m2 = not $ m1 == m2

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) Empty (Node x y z) = False
  (==) (Node x z w) Empty = False
  (==) (Node l1 v1 r1) (Node l2 v2 r2) = l1==l2 && v1==v2 && r1==r2

data Foo = Bar | Baz
