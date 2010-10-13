module ExpressionDisplay (displayExpr) where

import Expression

displayExpr :: Expression -> String
displayExpr (ExpressionInteger n) = show n
displayExpr (ExpressionVariable v) = v
