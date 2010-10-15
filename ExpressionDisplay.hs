module ExpressionDisplay (displayExpr) where

import Expression

displayExpr :: Expression -> String
displayExpr = fst . displayWithPrecedence

displayWithPrecedence :: Expression -> (String,Int)
displayWithPrecedence (ExpressionInteger n) = (show n,10)
displayWithPrecedence (ExpressionVariable v) = (v,10)
displayWithPrecedence (ExpressionSum (e:es)) =
  (foldl withplus (displayExpr e) es,0) where
    withplus :: String -> Expression -> String
    withplus s t = s ++ " + " ++ displayExpr t
displayWithPrecedence (ExpressionProduct (e:es)) =
  (foldl withspace (parenthesize (displayWithPrecedence e) 1) es,1) where
    withspace :: String -> Expression -> String
    withspace s t = s ++ " " ++ parenthesize (displayWithPrecedence t) 1

parenthesize :: (String,Int) -> Int -> String
parenthesize (str,x) y
  | x < y = "(" ++ str ++ ")"
  | otherwise = str
