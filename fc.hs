import System.Console.Readline

import Parser
import Tokenizer
import Expression
import ExpressionParser

mainloop :: IO ()
mainloop = do i <- readline "> "
              case i of
                Nothing -> return ()
                Just str -> do if str /= ""
                                 then addHistory str
                                 else return ()
                               case parseAll tokenizer str of
                                 Right tokens -> do case parseAll expressionParser tokens of
                                                      Right expr -> putStrLn $ show expr
                                                      Left err -> putStrLn "Error: unrecognized expression"
                                                    mainloop
                                 Left err -> do putStrLn $ "  " ++ replicate (errorLocation err) '-' ++ "^"
                                                putStrLn "Error: unrecognized input"
                                                mainloop

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          mainloop
