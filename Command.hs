module Command (Command(..), execute) where

import Expression
import ExpressionDisplay
import Store

data Command = CommandAssign String Expression |
               CommandEval Expression

execute :: Store -> Command -> (Store, String)
execute s (CommandAssign v e) = (setValue v e s, displayAssignment v e)
execute s (CommandEval e) =
  let Just (ExpressionInteger n) = getValue "#" s
      var = "%" ++ show n
      result = standardForm $ substitute s e
  in
  (setValue "#" (eInt (n+1)) $
   setValue var result s, displayAssignment var result)

substitute :: Store -> Expression -> Expression
substitute s = eMatch
               eInt                           -- Integer -> Expression
               (get s)                        -- String -> Expression
               (eSum . (map $ substitute s))  -- [Expression] -> Expression
               (eProd . (map $ substitute s)) -- [Expression] -> Expression

get :: Store -> String -> Expression
get store str =
  case getValue str store of
    Just expr -> substitute store expr
    Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpr e
