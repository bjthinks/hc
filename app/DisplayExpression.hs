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
  undefined
  undefined undefined undefined b
  where
    displayVariableToPower v = v ++ "^" ++ show n  -- TODO not right if n<0

displayCall :: String -> [Expression] -> String
displayCall f [] = f ++ "()"
displayCall f xs  = f ++ "(" ++ intercalate ", " (map displayExpression xs) ++
  ")"
