module CommandParser (commandParser) where

import Parser
import Tokenizer
import ASTParser
import ExprFromAST
import Command

commandParser :: Parser Token Command
commandParser = assign ||| eval

assign :: Parser Token Command
assign = do TokenWord v <- pProp isWord
            _ <- pElt TokenAssign
            ast <- astExprParser
            _ <- pElt TokenEnd
            return $ CommandAssign v (fromAST ast)

eval :: Parser Token Command
eval = do ast <- astExprParser
          _ <- pElt TokenEnd
          return $ CommandEval (fromAST ast)
