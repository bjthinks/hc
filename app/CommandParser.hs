module CommandParser (commandParser) where

import Control.Applicative
import Parser
import Tokenizer
import ExpressionParser
import Command

commandParser :: Parser Token [Command]
commandParser = do
  c <- command
  cs <- many $ do _ <- match TokenSemicolon
                  command
  _ <- match TokenEnd
  return (c:cs)

command :: Parser Token Command
command = exit <|> assign <|> clear <|> help <|> eval <|> blank

exit :: Parser Token Command
exit = do _ <- match $ TokenWord "exit"
          return CommandExit

assign :: Parser Token Command
assign = do TokenWord v <- matching isWord
            _ <- match TokenAssign
            e <- expressionParser
            return $ CommandAssign v e

clear :: Parser Token Command
clear = do _ <- match $ TokenWord "clear"
           TokenWord v <- matching isWord
           vs <- many $ do _ <- match TokenComma
                           TokenWord x <- matching isWord
                           return x
           return $ CommandClear $ v:vs

help :: Parser Token Command
help = do _ <- match $ TokenWord "help"
          topic <- many $ matching oktopic
          return $ CommandHelp $ unwords $ map unToken topic
  where oktopic t = t /= TokenEnd && t /= TokenSemicolon

eval :: Parser Token Command
eval = do e <- expressionParser
          return $ CommandEval e

blank :: Parser Token Command
blank = return CommandBlank
