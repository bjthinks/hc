import System.Console.Readline

mainloop :: IO ()
mainloop = do i <- readline "> "
              case i of
                Nothing -> return ()
                Just str -> addHistory str >> main

main :: IO ()
main = do setCompletionEntryFunction $ Just $ \_ -> return []
          mainloop
