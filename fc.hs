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
                               let Right tokens = parseAll tokenizer str
                               let Right expr = parseAll expressionParser tokens
                               putStrLn $ show expr
                               mainloop

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          mainloop
