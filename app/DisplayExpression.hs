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
displayIntPow = undefined

displayCall :: String -> [Expression] -> String
displayCall f [] = f ++ "()"
displayCall f xs  = f ++ "(" ++ intercalate ", " (map displayExpression xs) ++
  ")"
