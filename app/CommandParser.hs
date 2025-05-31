module CommandParser (commandParser) where

import Control.Applicative
import Parser
import Tokenizer
import ASTParser
import Command

commandParser :: Parser Token [Command]
commandParser = do
  c <- command
  cs <- many $ do _ <- match TokenSemicolon
                  command
  _ <- match TokenEnd
  return (c:cs)

command :: Parser Token Command
command = assign <|> clear <|> eval

assign :: Parser Token Command
assign = do TokenWord v <- matching isWord
            _ <- match TokenAssign
            ast <- astParser
            return $ CommandAssign v ast

clear :: Parser Token Command
clear = do _ <- match $ TokenWord "clear"
           TokenWord v <- matching isWord
           return $ CommandClear v

eval :: Parser Token Command
eval = do ast <- astParser
          return $ CommandEval ast
