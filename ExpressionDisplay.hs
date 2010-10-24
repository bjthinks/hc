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

displayRational :: Rational -> (String,Int)
displayRational n
  | denominator n == 1 = (show (numerator n),10)
  | otherwise          = (show (numerator n) ++ " / " ++
                          show (denominator n),1)

displayVariable :: String -> (String,Int)
displayVariable v = (v,10)

displaySum :: [Expression] -> (String,Int)
displaySum es =
  (foldl withspace f fs',0) where
    fs' = map expandSign fs
    expandSign :: String -> String
    expandSign ('-':cs) = '-':' ':cs
    expandSign cs = '+':' ':cs
    (f:fs) = map displayExpr constLastExprs
    constLastExprs = filter (not . isRational) es ++ filter isRational es

displayProduct :: [Expression] -> (String,Int)
displayProduct es =
  (minusSign ++ termsStr,1)
  where
    termsStr = case (numeratorTerms,denominatorTerms) of
      (_,[]) -> numeratorStr
      ([],_) -> "1 / " ++ denominatorStr
      _ -> numeratorStr ++ " / " ++ denominatorStr
    numeratorStr = termsToStr numeratorTerms
    denominatorStr = termsToStr denominatorTerms
    termsToStr = foldl1 withspace . map displayTerm
    displayTerm t = parenthesize (displayWithPrecedence t) 1
    (sign,numeratorTerms,denominatorTerms) = prodAsQuot es
    minusSign = case sign of
      (-1) -> "-"
      _    -> ""

displayIntPow :: Expression -> Integer -> (String,Int)
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
