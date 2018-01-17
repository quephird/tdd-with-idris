module Parser

import Data.String

public export
data Command = Add String
             | Get Integer
             | Size
             | Quit

export
parseCommand : (cmd : String) ->
               (args : String) ->
               Maybe Command
parseCommand "add" str   = Just $ Add str
parseCommand "get" index = case parsePositive index of
                             Just index' => Just $ Get index'
                             Nothing     => Nothing
parseCommand "size" _    = Just Size
parseCommand "quit" _    = Just Quit
parseCommand _ _         = Nothing

export
parse : String -> Maybe Command
parse input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)
