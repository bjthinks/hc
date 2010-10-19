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
  (foldl withspace f fs',0) where
    fs' = map expandSign fs
    expandSign :: String -> String
    expandSign ('-':cs) = '-':' ':cs
    expandSign cs = '+':' ':cs
    (f:fs) = map displayExpr intLastExprs
    intLastExprs = filter isNonConstant es ++ filter isConstant es

displayProduct es = let
  rest = foldl withspace f fs
  (f:fs) = map displayTerm (filter isNonConstant es)
  displayTerm :: Expression -> String
  displayTerm e = parenthesize (displayWithPrecedence e) 1
  in case filter isConstant es of
    [] -> (rest,1)
    [c] -> case eMatch id (error "") (error "") (error "") c of
      (-1) -> ("-" ++ rest,1)
      cc -> (show cc ++ " " ++ rest,1)

withspace :: String -> String -> String
withspace s t = s ++ " " ++ t

parenthesize :: (String,Int) -> Int -> String
parenthesize (str,x) y
  | x < y = "(" ++ str ++ ")"
  | otherwise = str

isConstant :: Expression -> Bool
isConstant = eMatch fTrue fFalse fFalse fFalse

isNonConstant :: Expression -> Bool
isNonConstant = not . isConstant

fTrue :: a -> Bool
fTrue = first True

fFalse :: a -> Bool
fFalse = first False

first :: a -> b -> a
first x _ = x
