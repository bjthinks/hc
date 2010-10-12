module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression

expressionParser :: Parser Token Expression
expressionParser = integer ||| variable

integer :: Parser Token Expression
integer = do s <- pMaybe $ pElt TokenMinus
             TokenInteger n <- pProp isInteger
             return $ ExpressionInteger $ case s of
               Nothing -> n
               Just _ -> (-n)

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ ExpressionVariable w
