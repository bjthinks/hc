module ExpressionDisplay (displayExpr) where

import Expression
import ASTFromExpr
import ASTDisplay

displayExpr :: Expression -> String
displayExpr = astDisplay . fromExpr

{-
import Expression
import Data.Ratio

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
-}
