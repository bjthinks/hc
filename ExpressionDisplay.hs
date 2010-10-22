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
  case eMatch isNegOne fFalse fFalse fFalse (\_ -> fFalse) (head es) of
    True -> ('-':displayProduct' (tail es),1)
    False -> (displayProduct' es,1)

-- Display a product, without concern for initial constant special cases
displayProduct' es =
  case (numeratorTerms,denominatorTerms) of
    (_,[]) -> numeratorStr
    ([],_) -> "1 / " ++ denominatorStr
    _ -> numeratorStr ++ " / " ++ denominatorStr
    where
      numeratorTerms :: [Expression]
      numeratorTerms = filter (not . isNegPow) es
      denominatorTerms :: [Expression]
      denominatorTerms = filter isNegPow es
      denominatorTermsFlipped :: [Expression]
      denominatorTermsFlipped = map (flip eIntPow (-1)) denominatorTerms
      displayTerm :: Expression -> String
      displayTerm e = parenthesize (displayWithPrecedence e) 1
      displayTerms :: [Expression] -> [String]
      displayTerms = map displayTerm
      joinTerms :: [String] -> String
      joinTerms es = case es of
        [] -> ""
        [s] -> s
        (s:ss) -> foldl withspace s ss
      numeratorStr :: String
      numeratorStr = joinTerms (displayTerms numeratorTerms)
      denominatorStr :: String
      denominatorStr = joinTerms (displayTerms denominatorTermsFlipped)

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
