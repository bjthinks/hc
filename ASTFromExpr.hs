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
fromExprSum (e:es) =
  case isRational e of
    False -> foldl (\x y -> (case fromExpr y of
                                ASTNegation z -> ASTDifference x z
                                z -> ASTSum x z)) (fromExpr e) es
    True -> case fromExpr e of
      ASTNegation c -> ASTDifference (fromExprSum es) c
      c -> ASTSum (fromExprSum es) c

fromExprProd :: [Expression] -> ASTExpr
fromExprProd es = signop $
                  (case (nt,dt) of
                      (_,[]) -> fromExprProd' nt
                      ([],_) -> ASTQuotient (ASTInteger 1) (fromExprProd' dt)
                      _ -> ASTQuotient (fromExprProd' nt) (fromExprProd' dt))
  where
    fromExprProd' (p:ps) = foldl (\x y -> ASTProduct x $ fromExpr y)
                           (fromExpr p) ps
    signop = case s of
      1 -> id
      (-1) -> ASTNegation
    (s, nt, dt) = prodAsQuot es

fromExprIntPow :: Expression -> Integer -> ASTExpr
fromExprIntPow e n =
  case signum n of
    1 -> ASTPower (fromExpr e) (ASTInteger n)
    (-1) -> case n of
      (-1) -> ASTQuotient (ASTInteger 1) (fromExpr e)
      _ -> ASTQuotient (ASTInteger 1) (ASTPower (fromExpr e) (ASTInteger (-n)))
