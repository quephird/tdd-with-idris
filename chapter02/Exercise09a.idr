module Exercise09a

import Exercise03

showPalindrome : String -> String
showPalindrome str = (show $ palindrome str) ++ "\n"

main : IO ()
main = repl "Enter a string to check if it's a palindrome: "
       showPalindrome
