module Exercise04

||| Checks to see if the input string is palindromic;
||| function is now case-insensitive but also demands
||| that the input has more than ten characters.
public export palindrome : String -> Bool
palindrome str =
  let
    str' = toLower str
  in
    str' == (reverse str') && (length str' > 10)
