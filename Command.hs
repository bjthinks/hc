module Command (Command(..), execute) where

import Expression
import ExpressionDisplay
import Store

data Command = CommandAssign String Expression |
               CommandEval Expression

execute :: Store -> Command -> (Store, String)
execute s (CommandAssign v e) = (setValue v e s, displayAssignment v e)
execute s (CommandEval e) = (s, displayExpr (substitute s e))

substitute :: Store -> Expression -> Expression
substitute s = eMatch
               eInt                           -- Integer -> Expression
               (get s)                        -- String -> Expression
               (eSum . (map $ substitute s))  -- [Expression] -> Expression
               (eProd . (map $ substitute s)) -- [Expression] -> Expression
               (ePower . (pairmap $ substitute s))

pairmap :: (a -> b) -> (a,a) -> (b,b)
pairmap f (x,y) = (f x,f y)

get :: Store -> String -> Expression
get store str =
  case getValue str store of
    Just expr -> substitute store expr
    Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpr e
