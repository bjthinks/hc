module Expression (Expression(..), standardForm) where

import Data.List

data Expression = ExpressionInteger Integer |
                  ExpressionVariable String |
                  ExpressionSum [Expression]
                  deriving (Show, Eq)

-- Put an expression into "standard form".  This performs a series
-- of internal and basic algebraic simplifications, including:
-- sums are sorted
-- sums of constants are evaluated
standardForm :: Expression -> Expression
standardForm e = addConstants $ sortSums e

sortSums :: Expression -> Expression
sortSums s@(ExpressionSum []) = s
sortSums (ExpressionSum es) = ExpressionSum (sortBy sumOrder es') where
  es' = map sortSums es
  sumOrder :: Expression -> Expression -> Ordering
  sumOrder (ExpressionInteger _) (ExpressionInteger _) = EQ
  sumOrder (ExpressionVariable x) (ExpressionVariable y) = compare x y
  sumOrder (ExpressionInteger _) (ExpressionVariable _) = GT
  sumOrder (ExpressionVariable _) (ExpressionInteger _) = LT
  sumOrder (ExpressionSum _) (ExpressionSum _) = EQ
  sumOrder (ExpressionSum _) _ = GT
  sumOrder _ (ExpressionSum _) = LT
sortSums e@(_) = e

addConstants :: Expression -> Expression
addConstants (ExpressionSum es) = ExpressionSum (addConstants' es) where
  addConstants' (ExpressionInteger m:ExpressionInteger n:es) =
    addConstants' (ExpressionInteger (m+n):es)
  addConstants' (e:es) = e:addConstants' es
  addConstants' [] = []
addConstants e@(_) = e
