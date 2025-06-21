module Store(Store, newStore, setValue, clearValues, getValue,
             getVariables, nextResult, incrementResult) where

import Expression
import qualified Data.Map.Strict as Map

data Store =
  Store { values :: Map.Map String Expression
        , resultNumber :: Int }

newStore :: Store
newStore = Store { values = Map.empty, resultNumber = 1 }

setValue :: String -> Expression -> Store -> Store
setValue v e s = s { values = Map.insert v e $ values s }

clearValue :: String -> Store -> (Store, String)
clearValue v s = case Map.lookup v (values s) of
  Nothing -> (s, "Variable " ++ v ++ " has no definition.")
  Just _ -> (s { values = Map.delete v $ values s },
             "Removed definition of " ++ v ++ ".")

clearValues :: [String] -> Store -> (Store, String)
clearValues [] _ = undefined
clearValues [v] s = clearValue v s
clearValues (v:vs) s = (s'', msg ++ "\n" ++ msgs)
  where
    (s', msg) = clearValue v s
    (s'', msgs) = clearValues vs s'

getValue :: String -> Store -> Maybe Expression
getValue v s = Map.lookup v $ values s

getVariables :: Store -> [String]
getVariables s = Map.keys $ values s

nextResult :: Store -> String
nextResult s = "r" ++ show (resultNumber s)

incrementResult :: Store -> Store
incrementResult s = s { resultNumber = resultNumber s + 1 }
