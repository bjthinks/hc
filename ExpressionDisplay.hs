module ExpressionDisplay (displayExpr) where

import Expression

displayExpr :: Expression -> String
displayExpr = fst . displayWithPrecedence

displayWithPrecedence :: Expression -> (String,Int)
displayWithPrecedence = eMatch
                        displayInteger
                        displayVariable
                        displaySum
                        displayProduct

displayInteger n = (show n,10)
displayVariable v = (v,10)
displaySum (e:es) =
  (foldl withplus (displayExpr e) es,0) where
    withplus :: String -> Expression -> String
    withplus s t = s ++ " + " ++ displayExpr t
displayProduct es =
  (foldl withspace f fs,1) where
    (f:fs) = map displayTerm intFirstExprs
    intFirstExprs = filter isConstant es ++ filter isNonConstant es
    displayTerm :: Expression -> String
    displayTerm e = parenthesize (displayWithPrecedence e) 1
    withspace :: String -> String -> String
    withspace s t = s ++ " " ++ t
    isConstant :: Expression -> Bool
    isConstant (ExpressionInteger _) = True
    isConstant _ = False
    isNonConstant :: Expression -> Bool
    isNonConstant = not . isConstant

parenthesize :: (String,Int) -> Int -> String
parenthesize (str,x) y
  | x < y = "(" ++ str ++ ")"
  | otherwise = str
