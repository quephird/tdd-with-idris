module Exercise07

public export top_ten : Ord a => List a -> List a
top_ten lst = take 10 $ reverse $ sort lst
