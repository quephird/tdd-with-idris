module Main

import System

-- Exercises 5.2.1, 5.2.2, and 5.2.3

readNumber : IO (Maybe Nat)
readNumber = do putStr "Enter a number: "
                input <- getLine
                case all isDigit $ unpack input of
                  False => pure Nothing
                  True => pure $ Just $ cast input

guess : (target : Nat) -> (guessCount : Nat) -> IO ()
guess target guessCount = do formatGuesses
                             Just numOk <- readNumber | Nothing => tryAgain "Not a number!"
                             case compare numOk target of
                               LT => tryAgain "Too low!"
                               GT => tryAgain "Too high!"
                               EQ => putStrLn "ZOMG YOU WIN!!!"
                          where tryAgain : (message : String) -> IO ()
                                tryAgain message = do putStrLn message
                                                      guess target $ S guessCount
                                formatGuesses : IO ()
                                formatGuesses = putStrLn $ (show guessCount) ++ " guess(es) so far"


main : IO ()
main = do currentTimeSeconds <- time
          let randomNumber = fromIntegerNat $ mod currentTimeSeconds 100
          guess randomNumber 0
