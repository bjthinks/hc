module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as C

main :: IO ()
main = do (threadDelay 1000000 >> return ()) `C.catch` (\e -> print (e::C.AsyncException))
          main
