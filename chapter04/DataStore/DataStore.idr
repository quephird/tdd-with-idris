module DataStore

import Data.Vect

public export
data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

export
size : DataStore -> Nat
size (MkData size' _) = size'

export
items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

export
emptyStore : DataStore
emptyStore = MkData 0 []

-- ACHTUNG! This implementation is slightly different than the text;
-- as of 1.0, the Idris type checker doesn't appear to like shadowing
-- of variable names. See https://github.com/idris-lang/Idris-dev/issues/4268
export
addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem =
  MkData (S size) (addToItems items) where
    addToItems : Vect n String -> Vect (S n) String
    addToItems [] = [newItem]
    addToItems (item' :: items') = item' :: addToItems items'

export
getEntry : (idx : Integer) ->
           (store : DataStore) ->
           Maybe (String, DataStore)
getEntry idx store =
  let current_items = items store in
    case integerToFin idx (size store) of
      Nothing  => Just ("Index out of range", store)
      Just idx => Just (index idx current_items, store)

export
searchStore : (str : String) ->
              (store : DataStore) ->
              List String
searchStore str store = filter (isInfixOf "a") $ toList $ items store
