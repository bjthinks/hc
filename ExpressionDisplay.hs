module ExpressionDisplay (display) where

import Expression

display :: Command -> String
display (CommandAssign v e) = v ++ " := " ++ displayExpr e
display (CommandEval e) = displayExpr e

displayExpr :: Expression -> String
displayExpr (ExpressionInteger n) = show n
displayExpr (ExpressionVariable v) = v
