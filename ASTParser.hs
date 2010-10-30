module ASTParser (test_ASTParser) where

import Parser
import Tokenizer
import AST
--import Control.Monad
import Test.HUnit

{-
astExprParser :: Parser Token ASTExpr
astExprParser = additive

additive :: Parser Token ASTExpr
additive = do a <- multiplicative
              as <- pStar (do op <- pElt TokenPlus ||| pElt TokenMinus
                              rhs <- multiplicative
                              return (op,rhs))
              return $ makeAdditive a as
  where
    makeAdditive :: ASTExpr -> [(Token,ASTExpr)] -> ASTExpr
    makeAdditive a [] = a
    makeAdditive a ((TokenPlus,b):bs) = makeAdditive (ASTPlus a b) bs
    makeAdditive a ((TokenMinus,b):bs) = makeAdditive (ASTMinus a b) bs

multiplicative :: Parser Token ASTExpr
multiplicative = do a <- unary
                    as <- pStar (do op <- pElt TokenTimes ||| pElt TokenDivide
                                    rhs <- unary
                                    return (op,rhs))
                    return $ makeMultiplicative a as
  where
    makeMultiplicative :: ASTExpr -> [(Token,ASTExpr)] -> ASTExpr
    makeMultiplicative a [] = a
    makeMultiplicative a ((TokenTimes,b):bs) =
      makeMultiplicative (ASTTimes a b) bs
    makeMultiplicative a ((TokenDivide,b):bs) =
      makeMultiplicative (ASTDivide a b) bs

unary :: Parser Token ASTExpr
unary = do sign <- pMaybe $ pElt TokenMinus
           a <- power
           return $ case sign of
             Nothing -> a
             Just TokenMinus -> ASTNegative a
-}

{-
intpow :: Parser Token Expression
intpow = do b <- atom
            e <- pMaybe (do pElt TokenPower
                            sign <- pMaybe $ pElt TokenMinus
                            TokenInteger val <- pProp isInteger
                            return $ case sign of
                              Nothing -> val
                              Just TokenMinus -> (-val))
            return $ case e of
              Nothing -> b
              Just ee -> eIntPow b ee

atom :: Parser Token Expression
atom = integer ||| call ||| variable ||| paren

call :: Parser Token Expression
call = do TokenWord func <- pProp isWord
          pElt TokenOpenParen
          arg <- additive
          pElt TokenCloseParen
          case func of
            "expand" -> return $ expand arg
            _ -> mzero

paren :: Parser Token Expression
paren = do pElt TokenOpenParen
           e <- additive
           pElt TokenCloseParen
           return e
-}

integer :: Parser Token ASTExpr
integer = do TokenInteger n <- pProp isInteger
             return $ ASTInteger n

variable :: Parser Token ASTExpr
variable = do TokenWord w <- pProp isWord
              return $ ASTVariable w


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

test_ASTParser = [
  parseAll variable [TokenWord "abc"] ~?= Right (ASTVariable "abc"),
  parseAll variable [TokenWord "Aa1"] ~?= Right (ASTVariable "Aa1"),
  isLeft (parseAll variable [TokenInteger 7]) ~?= True,
  isLeft (parseAll variable [TokenPlus]) ~?= True,
  isLeft (parseAll variable [TokenMinus]) ~?= True,
  isLeft (parseAll variable [TokenTimes]) ~?= True,
  isLeft (parseAll variable [TokenDivide]) ~?= True,
  isLeft (parseAll variable [TokenPower]) ~?= True,
  isLeft (parseAll variable [TokenOpenParen]) ~?= True,
  isLeft (parseAll variable [TokenCloseParen]) ~?= True,
  isLeft (parseAll variable [TokenAssign]) ~?= True,
  isLeft (parseAll variable [TokenEnd]) ~?= True,

  isLeft (parseAll integer [TokenWord "abc"]) ~?= True,
  parseAll integer [TokenInteger 7] ~?= Right (ASTInteger 7),
  parseAll integer [TokenInteger (345)] ~?= Right (ASTInteger 345),
  isLeft (parseAll integer [TokenPlus]) ~?= True,
  isLeft (parseAll integer [TokenMinus]) ~?= True,
  isLeft (parseAll integer [TokenTimes]) ~?= True,
  isLeft (parseAll integer [TokenDivide]) ~?= True,
  isLeft (parseAll integer [TokenPower]) ~?= True,
  isLeft (parseAll integer [TokenOpenParen]) ~?= True,
  isLeft (parseAll integer [TokenCloseParen]) ~?= True,
  isLeft (parseAll integer [TokenAssign]) ~?= True,
  isLeft (parseAll integer [TokenEnd]) ~?= True
  ]
