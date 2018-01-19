module Main

import DataStore
import Parser

replWith' : (state : DataStore) ->
            (message : String) ->
            (handler : DataStore -> String -> Maybe (String, DataStore))
            -> IO ()
replWith' state message handler =
  replWith state message' handler where
    message' = "\n" ++ message

handleInvalid : DataStore -> Maybe (String, DataStore)
handleInvalid store = Just ("Invalid command", store)

handleAdd : String -> DataStore -> Maybe (String, DataStore)
handleAdd item store = Just ("ID: " ++ (show $ size store), addToStore store item)

handleSearch : String -> DataStore -> Maybe (String, DataStore)
handleSearch string store =
  let results = searchStore string store in
  Just (unlines $ map formatResult results, store) where
    formatResult : (Integer, String) -> String
    formatResult (idx, item) = show idx ++ ": " ++ item

handleSize : DataStore -> Maybe (String, DataStore)
handleSize store = Just ((show $ size store) ++ " item(s)", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse input of
    Nothing           => handleInvalid store
    Just (Add item)   => handleAdd item store
    Just (Search str) => handleSearch str store
    Just (Get idx)    => getEntry idx store
    Just Size         => handleSize store
    Just Quit         => Nothing

main : IO ()
main = replWith' emptyStore "Command: " processInput
