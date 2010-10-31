module ASTParser (test_ASTParser) where

import Parser
import Tokenizer
import AST
--import Control.Monad
import Test.HUnit

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
    makeAdditive a ((TokenPlus,b):bs) = makeAdditive (ASTSum a b) bs
    makeAdditive a ((TokenMinus,b):bs) = makeAdditive (ASTDifference a b) bs

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
      makeMultiplicative (ASTProduct a b) bs
    makeMultiplicative a ((TokenDivide,b):bs) =
      makeMultiplicative (ASTQuotient a b) bs

unary :: Parser Token ASTExpr
unary = do sign <- pMaybe $ pElt TokenMinus
           a <- power
           return $ case sign of
             Nothing -> a
             Just TokenMinus -> ASTNegation a

power :: Parser Token ASTExpr
power = do b <- atom
           e <- pMaybe (do pElt TokenPower
                           unary)
           return $ case e of
             Nothing -> b
             Just ee -> ASTPower b ee

atom :: Parser Token ASTExpr
atom = integer ||| call ||| variable ||| paren

call :: Parser Token ASTExpr
call = do TokenWord func <- pProp isWord
          pElt TokenOpenParen
          arg <- additive
          pElt TokenCloseParen
          return (ASTCall func [arg])

paren :: Parser Token ASTExpr
paren = do pElt TokenOpenParen
           e <- additive
           pElt TokenCloseParen
           return e

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
  isLeft (parseAll integer [TokenEnd]) ~?= True,

  parseAll multiplicative [TokenInteger 1,TokenTimes,TokenInteger 2,
                           TokenTimes, TokenInteger 3] ~?=
  Right (ASTProduct (ASTProduct (ASTInteger 1) (ASTInteger 2)) (ASTInteger 3)),
  parseAll multiplicative [TokenInteger 1,TokenDivide,TokenInteger 2,
                           TokenDivide, TokenInteger 3] ~?=
  Right (ASTQuotient (ASTQuotient (ASTInteger 1) (ASTInteger 2))
         (ASTInteger 3)),
  parseAll multiplicative
  [TokenInteger 1,TokenDivide,TokenInteger 2,TokenTimes,TokenInteger 3,
   TokenDivide,TokenInteger 4] ~?=
  Right (ASTQuotient (ASTProduct (ASTQuotient (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3)) (ASTInteger 4)),
  parseAll multiplicative
  [TokenMinus,TokenInteger 1,TokenDivide,TokenMinus,TokenInteger 2,TokenTimes,
   TokenInteger 3,TokenDivide,TokenMinus,TokenInteger 4] ~?=
  Right (ASTQuotient (ASTProduct (ASTQuotient (ASTNegation (ASTInteger 1))
                                  (ASTNegation (ASTInteger 2)))
                      (ASTInteger 3)) (ASTNegation (ASTInteger 4))),

  parseAll additive [TokenInteger 1,TokenPlus,TokenInteger 2,
                     TokenPlus,TokenInteger 3] ~?=
  Right (ASTSum (ASTSum (ASTInteger 1) (ASTInteger 2)) (ASTInteger 3)),
  parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2,
                     TokenMinus,TokenInteger 3] ~?=
  Right (ASTDifference (ASTDifference (ASTInteger 1) (ASTInteger 2))
         (ASTInteger 3)),
  parseAll additive
  [TokenInteger 1,TokenMinus,TokenInteger 2,TokenPlus,TokenInteger 3,
   TokenMinus,TokenInteger 4] ~?=
  Right (ASTDifference (ASTSum (ASTDifference (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3)) (ASTInteger 4)),
  parseAll additive
  [TokenMinus,TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2,TokenPlus,
   TokenInteger 3,TokenMinus,TokenMinus,TokenInteger 4] ~?=
  Right (ASTDifference (ASTSum (ASTDifference (ASTNegation (ASTInteger 1))
                                (ASTNegation (ASTInteger 2)))
                        (ASTInteger 3)) (ASTNegation (ASTInteger 4))),
  parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2] ~?=
  Right (ASTDifference (ASTInteger 1) (ASTInteger 2)),
  parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2] ~?=
  Right (ASTDifference (ASTInteger 1) (ASTNegation (ASTInteger 2))),
  isLeft (parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenMinus,
                             TokenInteger 2]) ~?= True,
  parseAll additive [TokenMinus,TokenInteger 3] ~?=
  Right (ASTNegation (ASTInteger 3)),
  isLeft (parseAll additive [TokenMinus,TokenMinus,TokenInteger 3]) ~?= True,

  parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2] ~?=
  Right (ASTPower (ASTInteger 1) (ASTInteger 2)),
  parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenInteger 2] ~?=
  Right (ASTNegation (ASTPower (ASTInteger 1) (ASTInteger 2))),
  parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenInteger 2] ~?=
  Right (ASTPower (ASTInteger 1) (ASTNegation (ASTInteger 2))),
  parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenMinus,
                  TokenInteger 2] ~?=
  Right (ASTNegation (ASTPower (ASTInteger 1) (ASTNegation (ASTInteger 2)))),
  isLeft (parseAll unary [TokenMinus,TokenMinus,TokenInteger 1,TokenPower,
                          TokenInteger 2]) ~?= True,
  isLeft (parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenMinus,
                          TokenInteger 2]) ~?= True,
  parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2,TokenPower,
                  TokenInteger 3,TokenPower,TokenInteger 4] ~?=
  Right (ASTPower (ASTInteger 1)
         (ASTPower (ASTInteger 2)
          (ASTPower (ASTInteger 3)
           (ASTInteger 4)))),

  parseAll additive [TokenMinus,TokenOpenParen,TokenInteger 1,TokenPlus,
                     TokenInteger 2,TokenCloseParen,TokenPower,
                     TokenInteger 3] ~?=
  Right (ASTNegation (ASTPower (ASTSum (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3))),

  parseAll additive [TokenWord "a",TokenOpenParen,TokenWord "b",
                     TokenCloseParen] ~?=
  Right (ASTCall "a" [ASTVariable "b"]),
  parseAll additive [TokenWord "a",TokenOpenParen,TokenInteger 1,
                     TokenCloseParen] ~?= Right (ASTCall "a" [ASTInteger 1]),
  isLeft (parseAll additive [TokenInteger 1,TokenOpenParen,TokenInteger 2,
                             TokenCloseParen]) ~?= True,
  isLeft (parseAll additive [TokenInteger 1,TokenOpenParen,TokenWord "b",
                             TokenCloseParen]) ~?= True
  ]
