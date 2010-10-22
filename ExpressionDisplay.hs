module ExpressionDisplay (displayExpr) where

import Expression
import Data.Ratio

displayExpr :: Expression -> String
displayExpr = fst . displayWithPrecedence

displayWithPrecedence :: Expression -> (String,Int)
displayWithPrecedence = eMatch
                        displayRational
                        displayVariable
                        displaySum
                        displayProduct
                        displayIntPow

displayRational n
  | denominator n == 1 = (show (numerator n),10)
  | otherwise          = (show (numerator n) ++ " / " ++
                          show (denominator n),1)

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
  eatUnitNumerator ('1':' ':'/':cs) = '/':cs
  eatUnitNumerator cs = cs
  displayTerm :: Expression -> String
  displayTerm e = parenthesize (displayWithPrecedence e) 1
  displayProduct' es =
    let (f:fs) = map displayTerm es in
    foldl withspace f (map eatUnitNumerator fs)
  in case eMatch isNegOne fFalse fFalse fFalse (\_ -> fFalse) (head es) of
    True -> ('-':displayProduct' (tail es),1)
    False -> (displayProduct' es,1)

displayIntPow e n
  | n == (-1) = ("1 / " ++ parenthesize (displayWithPrecedence e) 2,1)
  | n < 0     = ("1 / " ++ parenthesize (displayWithPrecedence e) 2 ++
                 "^" ++ show (-n),1)
  | otherwise = (parenthesize (displayWithPrecedence e) 2 ++ "^" ++ show n,1)

withspace :: String -> String -> String
withspace s t = s ++ " " ++ t

parenthesize :: (String,Int) -> Int -> String
parenthesize (str,x) y
  | x < y = "(" ++ str ++ ")"
  | otherwise = str

isNegOne :: Rational -> Bool
isNegOne (-1) = True
isNegOne _ = False

isConstant :: Expression -> Bool
isConstant = eMatch fTrue fFalse fFalse fFalse (\_ -> fFalse)

isNonConstant :: Expression -> Bool
isNonConstant = not . isConstant

fTrue :: a -> Bool
fTrue = first True

fFalse :: a -> Bool
fFalse = first False

first :: a -> b -> a
first x _ = x
