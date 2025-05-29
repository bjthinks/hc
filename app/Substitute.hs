module Substitute (substitute) where

import Expression

substitute :: String -> Expression -> Expression -> Expression
substitute v e = eTransform eRat (substituteVariable v e) eSum eProd eIntPow

substituteVariable :: String -> Expression -> String -> Expression
substituteVariable v e w
  | v == w = e
  | otherwise = eVar w
