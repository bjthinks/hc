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
    nonConstTerms = filter isNonConstant es

{-
displayProduct es =
  case eMatch Just fNothing fNothing fNothing (\_ -> fNothing) (head es) of
    Nothing -> (displayProduct'
                (filter (not . isNegPow) es)
                (filter isNegPow es),1)
    Just c -> if c < 0 then
                ('-':displayProduct'
                 (eProd (eRat (-(numerator c%1)):filter (not . isNegPow) (tail es)))
                 (eProd (eRat (denominator c%1):filter isNegPow (tail es))),1)
              else
                (displayProduct'
                 (eProd (eRat (-(numerator c%1)):filter (not . isNegPow) (tail es)))
                 (eProd (eRat (denominator c%1):filter isNegPow (tail es))),1)


-- Display a quotient of two products, without concern for
-- negative powers or special-case constants
displayProduct' :: [Expression] -> [Expression] -> String
displayProduct' numeratorTerms denominatorTerms =
    where
      denominatorTermsFlipped :: [Expression]
      denominatorTermsFlipped = map (flip eIntPow (-1)) denominatorTerms
-}

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

isNegPow :: Expression -> Bool
isNegPow = eMatch fFalse fFalse fFalse fFalse (\_ -> \n -> n<0)

fTrue :: a -> Bool
fTrue = first True

fFalse :: a -> Bool
fFalse = first False

first :: a -> b -> a
first x _ = x
