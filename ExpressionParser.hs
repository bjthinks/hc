module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression

expressionParser :: Parser Token Expression
expressionParser = do e <- additive
                      pElt TokenEnd
                      return $ standardForm e

additive :: Parser Token Expression
additive = do a <- atom
              as <- pStar (pElt TokenPlus >> atom)
              return $ case as of
                [] -> a
                _ -> ExpressionSum (a:as)

atom :: Parser Token Expression
atom = integer ||| variable

integer :: Parser Token Expression
integer = do s <- pMaybe $ pElt TokenMinus
             TokenInteger n <- pProp isInteger
             return $ ExpressionInteger $ case s of
               Nothing -> n
               Just _ -> (-n)

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ ExpressionVariable w
