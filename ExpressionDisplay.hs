module ExpressionDisplay (display) where

import Expression

display :: Expression -> String
display (ExpressionInteger n) = show n
