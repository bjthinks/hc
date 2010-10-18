module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression

expressionParser :: Parser Token Expression
expressionParser = do e <- additive
                      pElt TokenEnd
                      return $ standardForm e

additive :: Parser Token Expression
additive = do a <- multiplicative
              as <- pStar (pElt TokenPlus >> multiplicative)
              return $ case as of
                [] -> a
                _ -> eSum (a:as)

multiplicative :: Parser Token Expression
multiplicative = do a <- atom
                    as <- pStar (pElt TokenTimes >> atom)
                    return $ case as of
                      [] -> a
                      _ -> eProd (a:as)

atom :: Parser Token Expression
atom = integer ||| variable ||| historyVariable ||| paren

integer :: Parser Token Expression
integer = do s <- pMaybe $ pElt TokenMinus
             TokenInteger n <- pProp isInteger
             return $ eInt $ case s of
               Nothing -> n
               Just _ -> (-n)

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ eVar w

historyVariable :: Parser Token Expression
historyVariable = do pElt TokenPercent
                     TokenInteger n <- pProp isInteger
                     return $ ExpressionVariable ('%':show n)

paren :: Parser Token Expression
paren = do pElt TokenOpenParen
           e <- additive
           pElt TokenCloseParen
           return e
