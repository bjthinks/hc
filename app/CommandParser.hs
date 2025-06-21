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
command = exit <|> clear <|> help <|> assignVariable <|> assignFunction <|>
  eval <|> blank

exit :: Parser Token Command
exit = do _ <- match $ TokenWord "exit"
          return CommandExit

assignVariable :: Parser Token Command
assignVariable = do
  TokenWord v <- matching isWord
  _ <- match TokenAssign
  e <- expressionParser
  return $ CommandAssignVariable v e

assignFunction :: Parser Token Command
assignFunction = do
  TokenWord f <- matching isWord
  _ <- match TokenOpenParen
  maybeVars <- option $ do TokenWord v <- matching isWord
                           vs <- many $ do _ <- match TokenComma
                                           TokenWord w <- matching isWord
                                           return w
                           return (v:vs)
  let vars = case maybeVars of
        Nothing -> []
        Just vs -> vs
  _ <- match TokenCloseParen
  _ <- match TokenAssign
  e <- expressionParser
  return $ CommandAssignFunction f vars e

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
