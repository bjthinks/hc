module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression

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
                                TokenMinus -> eProd [eInt (-1),term])
              return $ eSum (a:as)

multiplicative :: Parser Token Expression
multiplicative = do a <- unary
                    as <- pStar (pElt TokenTimes >> unary)
                    return $ eProd (a:as)

unary :: Parser Token Expression
unary = minus atom ||| atom

minus :: Parser Token Expression -> Parser Token Expression
minus p = do pElt TokenMinus
             expr <- p
             return $ eProd [eInt (-1),expr]

atom :: Parser Token Expression
atom = integer ||| variable ||| paren

integer :: Parser Token Expression
integer = do TokenInteger n <- pProp isInteger
             return $ eInt n

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ eVar w

paren :: Parser Token Expression
paren = do pElt TokenOpenParen
           e <- additive
           pElt TokenCloseParen
           return e
