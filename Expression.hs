module Expression (Expression(..)) where

data Expression = ExpressionInteger Integer |
                  ExpressionVariable String
                  deriving Show
