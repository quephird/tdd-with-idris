module Exercise03

||| Checks to see if the input string is palindromic;
||| function is now case-insensitive
public export palindrome : String -> Bool
palindrome str =
  let
    str' = toLower str
  in
    str' == (reverse str')
