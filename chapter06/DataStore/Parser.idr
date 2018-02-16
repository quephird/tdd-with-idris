module Parser

import DataStore
import Data.String

public export
data Command : Schema -> Type where
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Size : Command schema
     Quit : Command schema

-- Expects a list of characters to contain a double-quote at the beginning
-- and end of the list, returns Nothing if one or both are missing.
parseQuoted : List Char -> Maybe (String, String)
parseQuoted ('"' :: restOfInputChars) =
    case span (/= '"') restOfInputChars of
         (quotedString, '"' :: restOfInputChars') => Just (pack quotedString, ltrim $ pack restOfInputChars')
         (_, []) => Nothing
parseQuoted _           = Nothing

-- Parses a single token if the schema is a SString or SInt,
-- or if the schema is composite splits the input into two pieces
-- and attempts to parse left and right sides recursively.
parseToken : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema, String)
parseToken SChar input   = case unpack input of
                                [] => Nothing
                                c :: cs => Just (c, ltrim $ pack cs)
parseToken SString input = parseQuoted $ unpack input
parseToken SInt input    = case span isDigit input of
                                ("", _) => Nothing
                                (headOfInput, restOfInput) => Just (cast headOfInput, ltrim restOfInput)

parseToken (leftSchema .+. rightSchema) input = do (leftItem, input') <- parseToken leftSchema input
                                                   (rightItem, input'') <- parseToken rightSchema input'
                                                   Just ((leftItem, rightItem), input'')

-- For now, this expects input to be a string containing
-- numeric literals, correspondent with the SInt schema element,
-- and/or doubly-quoted strings, correspondent with SString.
-- For example, the string:
--
--     123 "123"
--
-- ... would be parsed as:
--
--     Just(123, "123") : Maybe (Int, String)
--
-- Any mismatched quotes or unquoted mixture of numerals and letters is
-- rejected and returned as Nothing.
parseNewItem : (schema : Schema) -> (input : String) -> Maybe $ SchemaType schema
parseNewItem schema input = case parseToken schema input of
                                 Just (parsedInput, "") => Just parsedInput
                                 Just _ => Nothing
                                 Nothing => Nothing

-- Expects an input schema to be composed only of "String"s and "Int"s;
-- anything else is rejected and returned as Nothing.
parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: rest) = case rest of
                                    [] => Just SChar
                                    _  => do restSchema <- parseSchema rest
                                             Just $ SChar .+. restSchema
parseSchema ("String" :: rest) = case rest of
                                      [] => Just SString
                                      _  => do restSchema <- parseSchema rest
                                               Just $ SString .+. restSchema
parseSchema ("Int" :: rest) = case rest of
                                   [] => Just SInt
                                   _  => do restSchema <- parseSchema rest
                                            Just $ SInt .+. restSchema
parseSchema _ = Nothing

-- Expects a command to be a keyword followed by zero or more string tokens
-- as arguments, and, for certain commands, the arguments have to be
-- consistent with the schema in question.
parseCommand : (schema : Schema) -> String -> String -> Maybe $ Command schema
parseCommand schema "schema" input = do newSchema <- parseSchema $ words input
                                        Just $ SetSchema newSchema
parseCommand schema "add" input    = do newItem <- parseNewItem schema input
                                        Just $ Add newItem
parseCommand schema "get" input    = do index <- parsePositive input
                                        Just $ Get index
parseCommand schema "size" _       = Just Size
parseCommand schema "quit" _       = Just Quit
parseCommand _ _ _                 = Nothing

-- Public interface for parsing user input supplied from the REPL
export
parse : (schema : Schema) -> String -> Maybe $ Command schema
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)
