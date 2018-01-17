module Exercise09b

import Exercise06

showCounts : String -> String
showCounts str = (show $ counts str) ++ "\n"

main : IO ()
main = repl "Enter a string to see the counts of letters and words: "
       showCounts
