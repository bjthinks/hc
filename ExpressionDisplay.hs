module ExpressionDisplay (execute) where

import Expression
import qualified Data.Map as Map

execute :: Map.Map String Expression -> Command -> (Map.Map String Expression, String)
execute s c@(CommandAssign v e) = (Map.insert v e s, display c)
execute s (CommandEval e) = (s, display $ CommandEval (substitute s e))

substitute :: Map.Map String Expression -> Expression -> Expression
substitute s (ExpressionInteger n) = ExpressionInteger n
substitute s (ExpressionVariable v) =
  case Map.lookup v s of
    Just e -> substitute s e
    Nothing -> ExpressionVariable v

display :: Command -> String
display (CommandAssign v e) = v ++ " := " ++ displayExpr e
display (CommandEval e) = displayExpr e

displayExpr :: Expression -> String
displayExpr (ExpressionInteger n) = show n
displayExpr (ExpressionVariable v) = v
