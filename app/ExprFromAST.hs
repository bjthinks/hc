module ExprFromAST (fromAST) where

import AST
import Expression
import Expand
import Data.Ratio ((%))
import HCException
import Control.Exception (throw)

fromAST :: ASTExpr -> Expression
fromAST (ASTInteger n) = eRat (n%1)
fromAST (ASTVariable v) = eVar v
fromAST (ASTSum x y) = eSum [fromAST x,fromAST y]
fromAST (ASTDifference x y) = eSum [fromAST x,eProd [eRat (-1%1),fromAST y]]
fromAST (ASTProduct x y) = eProd [fromAST x,fromAST y]
fromAST (ASTQuotient x y) = eProd [fromAST x,eIntPow (fromAST y) (-1)]
fromAST (ASTPower x (ASTInteger y)) = eIntPow (fromAST x) y
fromAST (ASTPower x (ASTNegation (ASTInteger y))) = eIntPow (fromAST x) (-y)
fromAST (ASTPower _ _) = throw HCNonIntegerPower
fromAST (ASTNegation x) = eProd [eRat (-1%1),fromAST x]
fromAST (ASTCall "expand" [x]) = expand (fromAST x)
fromAST (ASTCall f _) = error $ "Undefined function " ++ f
