module ExpressionDisplay (displayExpr) where

import Expression
import ASTFromExpr
import ASTDisplay

displayExpr :: Expression -> String
displayExpr = astDisplay . fromExpr
