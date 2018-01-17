module Picture

import Shape

public export
data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pict, pict1, pict2

export
pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pict1 pict2) = pictureArea pict1 + pictureArea pict2
pictureArea (Rotate angle pict) = pictureArea pict
pictureArea (Translate x y pict) = pictureArea pict
