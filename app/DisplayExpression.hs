module DisplayExpression (displayExpression) where

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
displayProduct = undefined

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
