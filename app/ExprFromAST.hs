module ExprFromAST (fromAST) where

import AST
import Expression
import Expand
import Substitute
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
fromAST (ASTCall "expand" [x]) = expand $ fromAST x
fromAST (ASTCall "expand" _) = throw $ HCWrongNumberOfParameters "expand" 1
fromAST (ASTCall "substitute" [ASTVariable f,a1,a2]) =
  substitute f (fromAST a1) (fromAST a2)
fromAST (ASTCall "substitute" [_,_,_]) = throw HCSubstituteNotVariable
fromAST (ASTCall "substitute" _) = throw $
  HCWrongNumberOfParameters "substitute" 3
fromAST (ASTCall f xs) = eCall f $ map fromAST xs
