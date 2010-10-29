module AST (ASTExpr(..)) where

data ASTExpr = ASTInteger Integer |
               ASTVariable String |
               ASTSum [ASTExpr] |
               ASTNegative ASTExpr |
               ASTProduct [ASTExpr] |
               ASTReciprocal ASTExpr |
               ASTExponential ASTExpr ASTExpr |
               ASTCall String [ASTExpr]
             deriving (Show)
