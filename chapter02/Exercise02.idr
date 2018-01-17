module Exercise02

||| Checks to see if the input string is palindromic
public export palindrome : String -> Bool
palindrome str = str == (reverse str)
