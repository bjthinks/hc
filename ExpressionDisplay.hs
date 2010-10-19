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
displaySum es =
  (foldl withplus f fs,0) where
    withplus :: String -> String -> String
    withplus s t = s ++ " + " ++ t
    (f:fs) = map displayExpr intLastExprs
    intLastExprs = filter isNonConstant es ++ filter isConstant es
    fTrue x = True
    fFalse x = False
    isConstant :: Expression -> Bool
    isConstant = eMatch fTrue fFalse fFalse fFalse
    isNonConstant :: Expression -> Bool
    isNonConstant = not . isConstant
displayProduct es =
  (foldl withspace f fs,1) where
    (f:fs) = map displayTerm es
    displayTerm :: Expression -> String
    displayTerm e = parenthesize (displayWithPrecedence e) 1
    withspace :: String -> String -> String
    withspace s t = s ++ " " ++ t

parenthesize :: (String,Int) -> Int -> String
parenthesize (str,x) y
  | x < y = "(" ++ str ++ ")"
  | otherwise = str
