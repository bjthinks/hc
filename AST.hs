module AST (ASTExpr(..)) where

data ASTExpr = ASTInteger Integer |
               ASTVariable String |
               ASTPlus ASTExpr ASTExpr |
               ASTMinus ASTExpr ASTExpr |
               ASTTimes ASTExpr ASTExpr |
               ASTDivide ASTExpr ASTExpr |
               ASTPower ASTExpr ASTExpr |
               ASTNegate ASTExpr |
               ASTCall String [ASTExpr]
             deriving (Eq, Show)
