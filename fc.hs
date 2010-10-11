import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT mainloop
          return ()

mainloop :: MaybeT IO ()
mainloop = do str <- MaybeT (readline "> ")
              liftIO $ processLine str
              mainloop

processLine :: String -> IO ()
processLine str = do if str /= ""
                       then addHistory str
                       else return ()
                     case parseAll tokenizer str of
                       Right tokens -> do case parseAll expressionParser tokens of
                                            Right expr -> putStrLn $ show expr
                                            Left err -> putStrLn "Error: unrecognized expression"
                       Left err -> do putStrLn $ "  " ++ replicate (errorLocation err) '-' ++ "^"
                                      putStrLn "Error: unrecognized input"
