import System.Console.Readline

import Parser
import Tokenizer

mainloop :: IO ()
mainloop = do i <- readline "> "
              case i of
                Nothing -> return ()
                Just str -> do if str /= ""
                                 then addHistory str
                                 else return ()
                               putStrLn $ show $ parseAll tokenizer str
                               mainloop

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          mainloop
