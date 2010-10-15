module ExpressionDisplay (displayExpr) where

import Expression

displayExpr :: Expression -> String
displayExpr (ExpressionInteger n) = show n
displayExpr (ExpressionVariable v) = v
displayExpr (ExpressionSum (e:es)) = foldl withplus (displayExpr e) es where
  withplus :: String -> Expression -> String
  withplus s t = s ++ " + " ++ displayExpr t
displayExpr (ExpressionProduct (e:es)) =
  foldl withspace (displayExpr e) es where
    withspace :: String -> Expression -> String
    withspace s t = s ++ " " ++ displayExpr t
