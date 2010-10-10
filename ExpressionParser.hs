module ExpressionParser (expressionParser) where

import Parser
import Tokenizer
import Expression

expressionParser :: Parser Token Expression
expressionParser = do s <- pMaybe $ pElt TokenMinus
                      TokenInteger n <- pProp isInteger
                      return $ ExpressionInteger $ case s of
                        Nothing -> n
                        Just _ -> (-n)