module DisplayExpression (displayExpression) where

import Data.Ratio
import Data.List
import Expression

displayExpression :: Expression -> String
displayExpression e = fst $ displayExpression' e

displayExpression' :: Expression -> (String, Int)
displayExpression' = eMatch
  (\r -> (displayRational r, 10))
  (\v -> (displayVar v, 10))
  (\es -> (displaySum es, 0))
  (\es -> (displayProduct es, 1))
  (\b e -> (displayIntPow b e, 3))
  (\f xs -> (displayCall f xs, 4))

displayExpressionWithPrecedence :: Expression -> String
displayExpressionWithPrecedence e = parenthesize $ displayExpression' e

parenthesize :: (String, Int) -> String
parenthesize (s, p)
  | p < 2 = "(" ++ s ++ ")"
  | otherwise = s

displayRational :: Rational -> String
displayRational r
  | d == 1 = show n
  | otherwise = show n ++ " / " ++ show d
  where
    n = numerator r
    d = denominator r

displayVar :: String -> String
displayVar = id

displaySum :: [Expression] -> String
displaySum [] = undefined
displaySum (e:es) = displayExpression e ++
  concat (map (adjustSign . displayExpression) es)
  where
    adjustSign ('-':cs) = " - " ++ cs
    adjustSign cs = " + " ++ cs

displayProduct :: [Expression] -> String
displayProduct es =
  if sign < 0 then "-" ++ numOverDen else numOverDen
  where
    (sign, numExprs, denExprs) = prodAsQuot es
    numOverDen
      | numExprs == [] && denExprs == [] = "1"
      | numExprs == [] = "1 / " ++ displaySimpleProduct denExprs
      | denExprs == [] = displaySimpleProduct numExprs
      | otherwise = displaySimpleProduct numExprs ++ " / " ++
                    displaySimpleProduct denExprs

 -- no denominator terms or negative constants anymore
displaySimpleProduct :: [Expression] -> String
displaySimpleProduct es =
  intercalate " " $ constant ++ map displayExpressionWithPrecedence fs
  where
    (fs, c) = extractConstantFromProduct es
    constant = if c == 1 then [] else [show $ numerator c]

displayIntPow :: Expression -> Integer -> String
displayIntPow b n = eMatch undefined displayVariableToPower
  displaySumToPower undefined undefined displayCallToPower b
  where
    displayVariableToPower v = raiseToPower v
    displaySumToPower es = raiseToPower $ "(" ++ displaySum es ++ ")"
    displayCallToPower f es = raiseToPower $ displayCall f es
    raiseToPower str
      | n == -1 = "1 / " ++ str
      | n < 0   = "1 / " ++ str ++ "^" ++ show (-n)
      | otherwise = str ++ "^" ++ show n

displayCall :: String -> [Expression] -> String
displayCall f xs  =
  f ++ "(" ++ intercalate ", " (map displayExpression xs) ++ ")"
