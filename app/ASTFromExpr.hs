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
fromExprSum [] = undefined
fromExprSum (e:es) = foldl combineTwoSummands (fromExpr e) es

combineTwoSummands :: ASTExpr -> Expression -> ASTExpr
combineTwoSummands e f = let f' = fromExpr f in
  case f' of
    ASTNegation g -> ASTDifference e g
    _ -> ASTSum e f'

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
      _ -> undefined
    (sign, numTerms, denTerms) = prodAsQuot es

-- Turn a list of expressions to be multiplied into an AST, assuming
-- no negative powers, denominators, reciprocals, or negative integers.
-- Assumes any whole numbers come last.
simpleProductToAST :: [Expression] -> ASTExpr
simpleProductToAST es = foldl makeProduct a as
  where
    xs = map fromExpr es
    a = head xs
    as = tail xs

makeProduct :: ASTExpr -> ASTExpr -> ASTExpr
makeProduct x y@(ASTInteger _) = ASTProduct y x
makeProduct x y = ASTProduct x y

fromExprIntPow :: Expression -> Integer -> ASTExpr
fromExprIntPow e n =
  case signum n of
    1 -> ASTPower (fromExpr e) (ASTInteger n)
    (-1) -> case n of
      (-1) -> ASTQuotient (ASTInteger 1) (fromExpr e)
      _ -> ASTQuotient (ASTInteger 1) (ASTPower (fromExpr e) (ASTInteger (-n)))
    _ -> undefined
