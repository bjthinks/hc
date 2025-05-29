module Store(Store, newStore, setValue, clearValue, getValue,
             getVariables) where

import Expression
import qualified Data.Map.Strict as Map

newtype Store = Store (Map.Map String Expression)

newStore :: Store
newStore = Store $ Map.empty

setValue :: String -> Expression -> Store -> Store
setValue v e (Store s) = Store $ Map.insert v e s

clearValue :: String -> Store -> Store
clearValue v (Store s) = Store $ Map.delete v s

getValue :: String -> Store -> Maybe Expression
getValue v (Store s) = Map.lookup v s

getVariables :: Store -> [String]
getVariables (Store s) = Map.keys s
