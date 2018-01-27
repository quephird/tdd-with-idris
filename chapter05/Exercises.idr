module Exercises

import Data.Vect

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

-- Exercise 5.3.1

readToBlank : IO (List String)
readToBlank = do string <- getLine
                 if string == ""
                   then pure []
                   else do strings <- readToBlank
                           pure $ string :: strings

-- Exercise 5.3.2

readAndSave : IO ()
readAndSave = do putStrLn "Enter a set of strings and then return when done:"
                 strings <- readToBlank
                 let output = unlines strings
                 Right _ <- writeFile "exercise_5_3_1.txt" output
                   | Left writeError  => putStrLn (show writeError)
                 putStrLn "File written successfully! \\o/"

-- Exercise 5.3.3

readVectFile' : (file : File) -> IO (n ** Vect n String)
readVectFile' file = do eof <- fEOF file
                        case eof of
                          True => do closeFile file
                                     pure (_ ** [])
                          False => do Right line <- fGetLine file
                                        | Left readError => pure (_ ** [])
                                      (_ ** moreLines) <- readVectFile' file
                                      pure (_ ** line :: moreLines)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read
                             | Left openError => pure (_ ** [])
                           do contents <- readVectFile' file
                              pure contents
