import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser
import ExpressionDisplay

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT mainloop
          putStrLn ""
          return ()

prompt :: String
prompt = "> "

mainloop :: MaybeT IO ()
mainloop = do str <- MaybeT (readline prompt)
              liftIO $ processLine str
              mainloop

processLine :: String -> IO ()
processLine str =
  case parseAll tokenizer str of
    Right [] -> return ()
    Right tokens -> do addHistory str
                       processTokens tokens
    Left err -> do addHistory str
                   printError (length prompt) (errorLocation err)
                     "unrecognized input"

processTokens :: [(Int,Token)] -> IO ()
processTokens tokens =
  case parseAll expressionParser (map snd tokens) of
    Right expr -> printResult expr
    Left err -> let stringLocation = fst $ tokens !! errorLocation err in
      printError (length prompt) stringLocation "unrecognized expression"

printResult :: Expression -> IO ()
printResult = putStrLn . display

printError :: Int -> Int -> String -> IO ()
printError s d m = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Error: " ++ m
    where
      spaces = replicate s ' '
      dashes = replicate d '-'
