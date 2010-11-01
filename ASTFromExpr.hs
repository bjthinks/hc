module ASTFromExpr (fromExpr) where

import Expression
import AST
import Data.Ratio (numerator,denominator)

fromExpr :: Expression -> ASTExpr
fromExpr =
  eMatch fromExprRat fromExprVar fromExprSum fromExprProd fromExprIntPow

fromExprRat :: Rational -> ASTExpr
fromExprRat c
  | c < 0 = ASTNegation $ fromExprRat (-c)
  | d == 1 = ASTInteger n
  | otherwise = ASTQuotient (ASTInteger n) (ASTInteger d)
    where
      n = numerator c
      d = denominator c

fromExprVar :: String -> ASTExpr
fromExprVar v = ASTVariable v

fromExprSum :: [Expression] -> ASTExpr
fromExprSum (e:es) = foldl (\x y -> ASTSum x $ fromExpr y) (fromExpr e) es

fromExprProd :: [Expression] -> ASTExpr
fromExprProd es = undefined

fromExprIntPow :: Expression -> Integer -> ASTExpr
fromExprIntPow e n = undefined
