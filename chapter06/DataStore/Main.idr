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

handleSetSchema : (store : DataStore) -> (newSchema : Schema) -> Maybe (String, DataStore)
handleSetSchema store newSchema = case size store of
                                       Z     => Just ("New database created!", emptyStore newSchema)
                                       (S k) => Just ("Sorry... data store is not empty", store)

handleAdd : (store : DataStore) -> SchemaType $ schema store -> Maybe (String, DataStore)
handleAdd store item = let newStore = addToStore store item in
                       Just ("ID: " ++ (show $ size store), newStore)

handleSize : DataStore -> Maybe (String, DataStore)
handleSize store = Just ((show $ size store) ++ " item(s)", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
       Nothing                    => handleInvalid store
       Just (SetSchema newSchema) => handleSetSchema store newSchema
       Just (Add item)            => handleAdd store item
       Just (Get idx)             => getEntry idx store
       Just Size                  => handleSize store
       Just Quit                  => Nothing

main : IO ()
main = replWith' (emptyStore SString) "Command: " processInput
