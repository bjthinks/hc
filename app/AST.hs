module AST (ASTExpr(..)) where

data ASTExpr = ASTInteger Integer |
               ASTVariable String |
               ASTSum ASTExpr ASTExpr |
               ASTDifference ASTExpr ASTExpr |
               ASTProduct ASTExpr ASTExpr |
               ASTQuotient ASTExpr ASTExpr |
               ASTPower ASTExpr ASTExpr |
               ASTNegation ASTExpr |
               ASTCall String [ASTExpr]
             deriving (Eq, Show)
