module Expression (Expression(..)) where

data Expression = ExpressionInteger Integer |
                  ExpressionVariable String |
                  ExpressionSum [Expression]
                  deriving (Show, Eq)

{-
-- Put an expression into "standard form".  This performs a series
-- of internal and basic algebraic simplifications, including:
-- sums are sorted
standardForm :: Expression -> Expression
standardForm e = sortSums e

sortSums :: Expression -> Expresssion
sortSums (ExpressionSum
-}
