module Shape

-- Exercises 7.1.1 and 7.1.2

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

%name Shape shape, shape1, shape2

export
area : Shape -> Double
area (Triangle w h) = 0.5 * w * h
area (Rectangle w h) = w * h
area (Circle r) = pi * r * r

public export
Eq Shape where
  (==) (Triangle w1 h1) (Triangle w2 h2) = w1==w2 && h1==h2
  (==) (Rectangle w1 h1) (Rectangle w2 h2) = w1==w2 && h1==h2
  (==) (Circle r1) (Circle r2) = r1==r2
  (==) _ _ = False

export
Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)
