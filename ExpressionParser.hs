module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression
import Data.Ratio
import Control.Monad
import Expand
import ExprFromAST
import ASTParser

expressionParser :: Parser Token Expression
expressionParser = do ast <- astExprParser
                      pElt TokenEnd
                      return (fromAST ast)

{-
expressionParser :: Parser Token Expression
expressionParser = do e <- additive
                      pElt TokenEnd
                      return e

additive :: Parser Token Expression
additive = do a <- multiplicative
              as <- pStar (do sign <- pElt TokenPlus ||| pElt TokenMinus
                              term <- multiplicative
                              return $ case sign of
                                TokenPlus -> term
                                TokenMinus -> eProd [eRat (-1),term])
              return $ eSum (a:as)

multiplicative :: Parser Token Expression
multiplicative = do a <- unary
                    as <- pStar (do inv <- pElt TokenTimes ||| pElt TokenDivide
                                    term <- unary
                                    return $ case inv of
                                      TokenTimes -> term
                                      TokenDivide -> eIntPow term (-1))
                    return $ eProd (a:as)

unary :: Parser Token Expression
unary = minus intpow ||| intpow

minus :: Parser Token Expression -> Parser Token Expression
minus p = do pElt TokenMinus
             expr <- p
             return $ eProd [eRat (-1),expr]

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

integer :: Parser Token Expression
integer = do TokenInteger n <- pProp isInteger
             return $ eRat (n%1)

call :: Parser Token Expression
call = do TokenWord func <- pProp isWord
          pElt TokenOpenParen
          arg <- additive
          pElt TokenCloseParen
          case func of
            "expand" -> return $ expand arg
            _ -> mzero

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ eVar w

paren :: Parser Token Expression
paren = do pElt TokenOpenParen
           e <- additive
           pElt TokenCloseParen
           return e
-}
