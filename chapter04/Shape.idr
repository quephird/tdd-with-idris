module Shape

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
