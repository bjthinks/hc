module Expression (Command(..), Expression(..)) where

data Command = CommandAssign String Expression |
               CommandEval Expression

data Expression = ExpressionInteger Integer |
                  ExpressionVariable String
                  deriving Show
