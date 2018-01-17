module Exercise06

||| Returns a tuple of the number of words and number of characters
public export counts :  String -> (Nat, Nat)
counts str = (length $ words str, length str)
