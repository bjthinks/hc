import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT mainloop
          putStrLn ""
          return ()

mainloop :: MaybeT IO ()
mainloop = do str <- MaybeT (readline "> ")
              liftIO $ processLine str
              mainloop

processLine :: String -> IO ()
processLine str =
  case parseAll tokenizer str of
    Right [] -> return ()
    Right tokens -> do addHistory str
                       processTokens tokens
    Left err -> do addHistory str
                   printError 2 (errorLocation err) "unrecognized input"

processTokens :: [(Int,Token)] -> IO ()
processTokens tokens =
  case parseAll expressionParser (map snd tokens) of
    Right expr -> putStrLn $ show expr
    Left err -> let stringLocation = fst $ tokens !! errorLocation err in
      printError 2 stringLocation "unrecognized expression"

printError :: Int -> Int -> String -> IO ()
printError s d m = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Error: " ++ m
    where
      spaces = replicate s ' '
      dashes = replicate d '-'
