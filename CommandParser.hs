module CommandParser (commandParser) where

import Parser
import Tokenizer
import ExpressionParser
import Command

commandParser :: Parser Token Command
commandParser = assign ||| eval

assign :: Parser Token Command
assign = do TokenWord v <- pProp isWord
            pElt TokenAssign
            e <- expressionParser
            return $ CommandAssign v e

eval :: Parser Token Command
eval = do e <- expressionParser
          return $ CommandEval e
