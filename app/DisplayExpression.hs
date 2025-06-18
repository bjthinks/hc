module DisplayExpression (displayExpression) where

import Data.Ratio
import Data.List
import Expression

displayExpression :: Expression -> String
displayExpression = eMatch displayRational id displaySum displayProduct
  displayIntPow displayCall

displayRational :: Rational -> String
displayRational = undefined

displaySum :: [Expression] -> String
displaySum = undefined

displayProduct :: [Expression] -> String
displayProduct es = if sign < 0 then "-" ++ numOverDen else numOverDen
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
displaySimpleProduct es = intercalate " " $ constant ++ map displayExpression fs
  where
    (fs, c) = extractConstantFromProduct es
    constant = if c == 1 then [] else [show $ numerator c]

displayIntPow :: Expression -> Integer -> String
displayIntPow b n = eMatch undefined displayVariableToPower
  displaySumToPower undefined undefined displayCallToPower b
  where
    displayVariableToPower v = raiseToPower v
    displaySumToPower es = raiseToPower $ "(" ++ displaySum es ++ ")"
    displayCallToPower f es = raiseToPower $ f ++ "(" ++
      intercalate "," (map displayExpression es) ++ ")"
    raiseToPower str
      | n == -1 = "1 / " ++ str
      | n < 0   = "1 / " ++ str ++ "^" ++ show (-n)
      | otherwise = str ++ "^" ++ show n

displayCall :: String -> [Expression] -> String
displayCall f xs  = f ++ "(" ++ intercalate ", " (map displayExpression xs) ++
  ")"
