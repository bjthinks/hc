module DisplayExpression (displayExpression) where

import Data.Ratio
import Data.List
import Expression

displayExpression :: Expression -> String
displayExpression e = fst $ displayExpression' e

displayExpression' :: Expression -> (String, Int)
displayExpression' = eMatch displayRational displayVar displaySum displayProduct
  displayIntPow displayCall

displayExpressionWithPrecedence :: Expression -> Int -> String
displayExpressionWithPrecedence e n =
  parenthesize (displayExpression' e) n

parenthesize :: (String, Int) -> Int -> String
parenthesize (s, p) q
  | p < q = "(" ++ s ++ ")"
  | otherwise = s

displayRational :: Rational -> (String, Int)
displayRational r
  | d == 1 = (show n, 10)
  | otherwise = (show n ++ " / " ++ show d, 1)
  where
    n = numerator r
    d = denominator r

displayVar :: String -> (String, Int)
displayVar x = (x, 10)

displaySum :: [Expression] -> (String, Int)
displaySum [] = undefined
displaySum (e:es) = (displayExpression e ++
  concat (map (adjustSign . displayExpression) es), 0)
  where
    adjustSign ('-':cs) = " - " ++ cs
    adjustSign cs = " + " ++ cs

displayProduct :: [Expression] -> (String, Int)
displayProduct es =
  (if sign < 0 then "-" ++ fst numOverDen else fst numOverDen, 1)
  where
    (sign, numExprs, denExprs) = prodAsQuot es
    numOverDen
      | numExprs == [] && denExprs == [] = ("1", 10)
      | numExprs == [] = ("1 / " ++ fst (displaySimpleProduct denExprs), 1)
      | denExprs == [] = displaySimpleProduct numExprs
      | otherwise = (fst (displaySimpleProduct numExprs) ++ " / " ++
                     fst (displaySimpleProduct denExprs), 1)

 -- no denominator terms or negative constants anymore
displaySimpleProduct :: [Expression] -> (String, Int)
displaySimpleProduct es =
  (intercalate " " $ constant ++
   map (flip displayExpressionWithPrecedence 2) fs, 2)
  where
    (fs, c) = extractConstantFromProduct es
    constant = if c == 1 then [] else [show $ numerator c]

displayIntPow :: Expression -> Integer -> (String, Int)
displayIntPow b n = eMatch undefined displayVariableToPower
  displaySumToPower undefined undefined displayCallToPower b
  where
    displayVariableToPower v = raiseToPower v
    displaySumToPower es = raiseToPower $ "(" ++ fst (displaySum es) ++ ")"
    displayCallToPower f es = raiseToPower $ f ++ "(" ++
      intercalate "," (map displayExpression es) ++ ")"
    raiseToPower str
      | n == -1 = ("1 / " ++ str, 1)
      | n < 0   = ("1 / " ++ str ++ "^" ++ show (-n), 1)
      | otherwise = (str ++ "^" ++ show n, 3)

displayCall :: String -> [Expression] -> (String, Int)
displayCall f xs  =
  (f ++ "(" ++ intercalate ", " (map displayExpression xs) ++ ")", 4)
