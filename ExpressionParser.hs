module ExpressionParser (commandParser) where

import Parser
import Tokenizer
import Expression

commandParser :: Parser Token Command
commandParser = assign ||| eval

assign :: Parser Token Command
assign = do TokenWord v <- pProp isWord
            pElt TokenAssign
            e <- expression
            return $ CommandAssign v e

eval :: Parser Token Command
eval = do e <- expression
          return $ CommandEval e


expression :: Parser Token Expression
expression = do e <- integer ||| variable
                return e

integer :: Parser Token Expression
integer = do s <- pMaybe $ pElt TokenMinus
             TokenInteger n <- pProp isInteger
             return $ ExpressionInteger $ case s of
               Nothing -> n
               Just _ -> (-n)

variable :: Parser Token Expression
variable = do TokenWord w <- pProp isWord
              return $ ExpressionVariable w
