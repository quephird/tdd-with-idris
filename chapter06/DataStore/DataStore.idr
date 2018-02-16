module DataStore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString
            | SInt
            | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

public export
record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size $ SchemaType schema

export
emptyStore : Schema -> DataStore
emptyStore schema = MkData schema 0 []

export
addToStore : (store : DataStore) ->
             SchemaType $ schema store ->
             DataStore
addToStore (MkData schema size items) newItem =
  MkData schema _ (addToItems items) where
    addToItems : Vect n $ SchemaType schema -> Vect (S n) $ SchemaType schema
    addToItems [] = [newItem]
    addToItems (item' :: items') = item' :: addToItems items'

displayItem : SchemaType schema -> String
displayItem {schema = SString} item = item
displayItem {schema = SInt}    item = show item
displayItem {schema = (x .+. y)} (iteml, itemr) = displayItem iteml ++ ", " ++ displayItem itemr

export
getEntry : (idx : Integer) ->
           (store : DataStore) ->
           Maybe (String, DataStore)
getEntry idx store =
  let current_items = items store in
    case integerToFin idx (size store) of
      Nothing  => Just ("Index out of range", store)
      Just idx => Just (displayItem $ index idx current_items, store)
--
-- export
-- searchStore : (str : String) ->
--               (store : DataStore) ->
--               List (Integer, String)
-- searchStore str store =
--   let itemsWithIndices = zip [0 .. toIntegerNat $ size store] $ toList $ items store in
--     filter isResult itemsWithIndices where
--       isResult : (Integer, String) -> Bool
--       isResult (idx, item) = isInfixOf str item
