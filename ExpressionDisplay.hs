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
    numeratorStr = joinTermStrs numeratorTermStrs
    denominatorStr = joinTermStrs denominatorTermStrs
    joinTermStrs (t:ts) = foldl withspace t ts
    numeratorTermStrs = displayTerms numeratorTerms
    denominatorTermStrs = displayTerms denominatorTerms
    displayTerms = map displayTerm
    displayTerm t = parenthesize (displayWithPrecedence t) 1
    getConstant = eMatch Just fNothing fNothing fNothing (\_ -> fNothing) $
                  head es
    fNothing _ = Nothing
    constant = case getConstant of
      Nothing -> 1
      Just c -> c
    minusSign = if constant < 0 then "-" else ""
    absConstant = abs constant
    constOfNum = numerator absConstant
    constOfDen = denominator absConstant
    numeratorTerms = case constOfNum of
      1 -> numeratorNonConstTerms
      _ -> eRat (constOfNum%1) : numeratorNonConstTerms
    denominatorTerms = case constOfDen of
      1 -> denominatorNonConstTerms
      _ -> eRat (constOfDen%1) : denominatorNonConstTerms
    numeratorNonConstTerms = filter (not . isNegPow) nonConstTerms
    denominatorNonConstTerms = map (flip eIntPow (-1)) $
                               filter isNegPow nonConstTerms
    nonConstTerms = filter (not . isRational) es

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
