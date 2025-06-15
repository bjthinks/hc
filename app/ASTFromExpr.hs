module ASTFromExpr (fromExpr) where

import Expression
import AST
import Data.Ratio (numerator,denominator)

fromExpr :: Expression -> AST
fromExpr =
  eMatch fromExprRat fromExprVar fromExprSum fromExprProd fromExprIntPow
    fromExprCall

fromExprRat :: Rational -> AST
fromExprRat c
  | c < 0 = ASTNegation $ fromExprRat (-c)
  | d == 1 = ASTInteger n
  | otherwise = ASTQuotient (ASTInteger n) (ASTInteger d)
    where
      n = numerator c
      d = denominator c

fromExprVar :: String -> AST
fromExprVar v = ASTVariable v

fromExprSum :: [Expression] -> AST
fromExprSum [] = undefined
fromExprSum (e:es) = foldl combineTwoSummands (fromExpr e) es

combineTwoSummands :: AST -> Expression -> AST
combineTwoSummands e f = let f' = fromExpr f in
  case f' of
    ASTNegation g -> ASTDifference e g
    _ -> ASTSum e f'

fromExprProd :: [Expression] -> AST
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
simpleProductToAST :: [Expression] -> AST
simpleProductToAST es = foldl makeProduct a as
  where
    xs = map fromExpr es
    a = head xs
    as = tail xs

makeProduct :: AST -> AST -> AST
makeProduct x y@(ASTInteger _) = ASTProduct y x
makeProduct x y = ASTProduct x y

fromExprIntPow :: Expression -> Integer -> AST
fromExprIntPow e n =
  case signum n of
    1 -> ASTPower (fromExpr e) (ASTInteger n)
    (-1) -> case n of
      (-1) -> ASTQuotient (ASTInteger 1) (fromExpr e)
      _ -> ASTQuotient (ASTInteger 1) (ASTPower (fromExpr e) (ASTInteger (-n)))
    _ -> undefined

fromExprCall :: String -> [Expression] -> AST
fromExprCall f xs = ASTCall f (map fromExpr xs)
