module Command (Command(..), execute) where

import Expression
import ExpressionDisplay
import Store

data Command = CommandAssign String Expression |
               CommandEval Expression

execute :: Store -> Command -> (Store, String)
execute s (CommandAssign v e) = (setValue v e s, displayAssignment v e)
execute s (CommandEval e) = (s, displayExpr (standardForm (substitute s e)))

substitute :: Store -> Expression -> Expression
substitute s (ExpressionInteger n) = ExpressionInteger n
substitute s (ExpressionVariable v) =
  case getValue v s of
    Just e -> substitute s e
    Nothing -> ExpressionVariable v
substitute s (ExpressionSum es) = ExpressionSum $ map (substitute s) es

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpr e
