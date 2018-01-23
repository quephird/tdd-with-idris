module Exercises

-- Exercise 5.1.1

printLonger : IO ()
printLonger = do putStr "Enter first string: "
                 x <- getLine
                 putStr "Enter second string: "
                 y <- getLine
                 putStrLn $ show $ max (length x) (length y)

-- Exercise 5.1.2

printLonger' : IO ()
printLonger' = putStr "Enter first string: " >>=
               \_ => getLine >>=
               \x => putStr "Enter second string: " >>=
               \_ => getLine >>=
               \y => putStrLn $ show $ max (length x) (length y)

-- Exercise 5.2.4

repl' : (prompt : String) ->
        (onInput : String -> String) ->
        IO ()
repl' prompt onInput = do putStr prompt
                          input <- getLine
                          let result = onInput input
                          putStr result
                          repl' prompt onInput

replWith' : (state : a) ->
            (prompt : String) ->
            (onInput : a -> String -> Maybe (String, a)) ->
            IO ()
replWith' state prompt onInput =
  do putStr prompt
     input <- getLine
     let result = onInput state input
     case result of
       Just (message, newState) => do putStr message
                                      replWith' newState prompt onInput
       Nothing => pure ()
