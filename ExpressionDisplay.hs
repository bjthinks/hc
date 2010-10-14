module ExpressionDisplay (displayExpr) where

import Expression

displayExpr :: Expression -> String
displayExpr (ExpressionInteger n) = show n
displayExpr (ExpressionVariable v) = v
displayExpr (ExpressionSum (e:es)) = foldl withplus (displayExpr e) es where
  withplus :: String -> Expression -> String
  withplus s t = s ++ " + " ++ displayExpr t
