module AST (AST(..)) where

data AST = ASTInteger Integer |
           ASTVariable String |
           ASTSum AST AST |
           ASTDifference AST AST |
           ASTProduct AST AST |
           ASTQuotient AST AST |
           ASTPower AST AST |
           ASTNegation AST |
           ASTCall String [AST]
  deriving (Eq, Show)
