module Main where

import Data.List (isPrefixOf, sort)
import Data.IORef
import System.Console.Haskeline
import Control.Monad.IO.Class

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:yy@(y:_))
  | x == y    = uniq yy
  | otherwise = x : uniq yy

varsBeginningWith :: IORef [String] -> String -> IO [Completion]
varsBeginningWith vars str = do
  currentVars <- readIORef vars
  return $ map simpleCompletion $ filter (isPrefixOf str) currentVars

completeVars :: IORef [String] -> CompletionFunc IO
completeVars vars =
  completeWord Nothing " \t\n\r" $ varsBeginningWith vars

mainloop :: IORef [String] -> InputT IO ()
mainloop vars =
  do minput <- getInputLine "% "
     case minput of
       Nothing -> return ()
       Just "quit" -> return ()
       Just input -> do outputStrLn $ "Input was: " ++ input
                        oldVars <- liftIO $ readIORef vars
                        let currentVars = uniq $ sort $ words input ++ oldVars
                        liftIO $ writeIORef vars currentVars
                        outputStrLn $ "vars are: " ++ unwords currentVars
                        mainloop vars

inputSettings :: IORef [String] -> Settings IO
inputSettings vars = setComplete (completeVars vars) defaultSettings

main :: IO ()
main = do vars <- newIORef []
          runInputT (inputSettings vars) (mainloop vars)
