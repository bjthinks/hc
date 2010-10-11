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
                       case parseAll expressionParser (map snd tokens) of
                         Right expr -> putStrLn $ show expr
                         Left err -> do putStrLn $ "  " ++ replicate (fst $ tokens !! errorLocation err) '-' ++ "^"
                                        putStrLn "Error: unrecognized expression"
    Left err -> do addHistory str
                   putStrLn $ "  " ++ replicate (errorLocation err) '-' ++ "^"
                   putStrLn "Error: unrecognized input"
