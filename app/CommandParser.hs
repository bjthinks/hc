module CommandParser (commandParser) where

import Control.Applicative
import Parser
import Tokenizer
import ASTParser
import ExprFromAST
import Command

commandParser :: Parser Token Command
commandParser = assign <|> clear <|> eval

assign :: Parser Token Command
assign = do TokenWord v <- matching isWord
            _ <- match TokenAssign
            ast <- astExprParser
            _ <- match TokenEnd
            return $ CommandAssign v (fromAST ast)

clear :: Parser Token Command
clear = do _ <- match $ TokenWord "clear"
           TokenWord v <- matching isWord
           _ <- match TokenEnd
           return $ CommandClear v

eval :: Parser Token Command
eval = do ast <- astExprParser
          _ <- match TokenEnd
          return $ CommandEval (fromAST ast)
