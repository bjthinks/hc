module Command (Command(..), execute) where

import Expression
import ExpressionDisplay
import qualified Data.Map as Map

data Command = CommandAssign String Expression |
               CommandEval Expression

execute :: Map.Map String Expression -> Command -> (Map.Map String Expression, String)
execute s (CommandAssign v e) = (Map.insert v e s, displayAssignment v e)
execute s (CommandEval e) = (s, displayExpr (substitute s e))

substitute :: Map.Map String Expression -> Expression -> Expression
substitute s (ExpressionInteger n) = ExpressionInteger n
substitute s (ExpressionVariable v) =
  case Map.lookup v s of
    Just e -> substitute s e
    Nothing -> ExpressionVariable v

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpr e
