module Expression (Expression(..), standardForm) where

import Data.List

data Expression = ExpressionInteger Integer |
                  ExpressionVariable String |
                  ExpressionSum [Expression] |
                  ExpressionProduct [Expression]
                  deriving (Show, Eq, Ord)

-- Put an expression into "standard form".  This performs a series
-- of internal and basic algebraic simplifications, including:
-- nested sums are flattened
-- sums are sorted
-- sums of constants are evaluated, and zeroes are removed
-- empty sums are replaced with zero, singleton sums are unwrapped
standardForm :: Expression -> Expression
standardForm e =
  let e' = removeTrivialSums $ addConstants $ sortSums $ flattenSums e in
  case e == e' of
    True -> e'
    False -> standardForm e'

flattenSums :: Expression -> Expression
flattenSums (ExpressionSum es) = ExpressionSum $ flattenSums' es' where
  es' = map flattenSums es
  flattenSums' :: [Expression] -> [Expression]
  flattenSums' (ExpressionSum nes:es) = flattenSums' (nes ++ es)
  flattenSums' (e:es) = e:flattenSums' es
  flattenSums' [] = []
flattenSums (ExpressionProduct es) = ExpressionProduct (map flattenSums es)
flattenSums e@(_) = e

sortSums :: Expression -> Expression
sortSums s@(ExpressionSum []) = s
sortSums (ExpressionSum es) = ExpressionSum (sort es)
sortSums (ExpressionProduct es) = ExpressionProduct (map sortSums es)
sortSums e@(_) = e

addConstants :: Expression -> Expression
addConstants (ExpressionSum es) = ExpressionSum (addConstants' es') where
  es' = map addConstants es
  addConstants' (ExpressionInteger 0:es) = addConstants' es
  addConstants' (ExpressionInteger m:ExpressionInteger n:es) =
    addConstants' (ExpressionInteger (m+n):es)
  addConstants' (e:es) = e:addConstants' es
  addConstants' [] = []
addConstants (ExpressionProduct es) = ExpressionProduct (map addConstants es)
addConstants e@(_) = e

removeTrivialSums :: Expression -> Expression
removeTrivialSums (ExpressionSum []) = ExpressionInteger 0
removeTrivialSums (ExpressionSum [e]) = removeTrivialSums e
removeTrivialSums (ExpressionSum es) = ExpressionSum $ map removeTrivialSums es
removeTrivialSums (ExpressionProduct es) =
  ExpressionProduct (map removeTrivialSums es)
removeTrivialSums e@(_) = e
