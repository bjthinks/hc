module Store(Store, newStore, setValue, getValue) where

import Expression
import qualified Data.Map as Map

newtype Store = StoreGuts (Map.Map String Expression)

newStore :: Store
newStore = StoreGuts $ Map.empty

setValue :: String -> Expression -> Store -> Store
setValue v e (StoreGuts s) = StoreGuts $ Map.insert v e s

getValue :: String -> Store -> Maybe Expression
getValue v (StoreGuts s) = Map.lookup v s
