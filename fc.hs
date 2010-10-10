import System.Console.Readline

main :: IO ()
main = do i <- readline "> "
          case i of
            Nothing -> return ()
            Just str -> main
