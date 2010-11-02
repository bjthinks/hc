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
fromExprProd es =
  signop $ case (numTerms,denTerms) of
    (_,[]) -> numAST
    ([],_) -> ASTQuotient (ASTInteger 1) denAST
    _ -> ASTQuotient numAST denAST
  where
    numAST = simpleProductToAST numTerms
    denAST = simpleProductToAST denTerms
    signop = case sign of
      1 -> id
      (-1) -> ASTNegation
    (sign, numTerms, denTerms) = prodAsQuot es

-- Turn a list of expressions to be multiplied into an AST, assuming
-- no negative powers, denominators, reciprocals, or minus signs.
simpleProductToAST :: [Expression] -> ASTExpr
simpleProductToAST (e:es) =
  foldl (\x y -> ASTProduct x (fromExpr y)) (fromExpr e) es

fromExprIntPow :: Expression -> Integer -> ASTExpr
fromExprIntPow e n =
  case signum n of
    1 -> ASTPower (fromExpr e) (ASTInteger n)
    (-1) -> case n of
      (-1) -> ASTQuotient (ASTInteger 1) (fromExpr e)
      _ -> ASTQuotient (ASTInteger 1) (ASTPower (fromExpr e) (ASTInteger (-n)))
