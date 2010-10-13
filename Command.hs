module Command (Command, commandParser, execute) where

import Parser
import Tokenizer
import Expression
import ExpressionParser
import ExpressionDisplay
import qualified Data.Map as Map

data Command = CommandAssign String Expression |
               CommandEval Expression

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

execute :: Map.Map String Expression -> Command -> (Map.Map String Expression, String)
execute s c@(CommandAssign v e) = (Map.insert v e s, display c)
execute s (CommandEval e) = (s, display $ CommandEval (substitute s e))

substitute :: Map.Map String Expression -> Expression -> Expression
substitute s (ExpressionInteger n) = ExpressionInteger n
substitute s (ExpressionVariable v) =
  case Map.lookup v s of
    Just e -> substitute s e
    Nothing -> ExpressionVariable v

display :: Command -> String
display (CommandAssign v e) = v ++ " := " ++ displayExpr e
display (CommandEval e) = displayExpr e
