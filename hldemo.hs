module Main where

import Data.List (isPrefixOf, sort)
import Data.IORef
import System.Console.Haskeline
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.Console.Haskeline.MonadException

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

inputSettings :: IORef [String] -> Settings IO
inputSettings vars = setComplete (completeVars vars) defaultSettings

quit :: MaybeT (InputT IO) ()
quit = MaybeT $ return Nothing

continue :: MaybeT (InputT IO) ()
continue = liftIO $ return ()

mainloop :: IORef [String] -> MaybeT (InputT IO) ()
mainloop vars = do input <- MaybeT $ getInputLine "% "
                   if input == "quit" then quit else continue
                   liftIO $ putStrLn $ "Input was: " ++ input
                   oldVars <- liftIO $ readIORef vars
                   let currentVars = uniq $ sort $ words input ++ oldVars
                   liftIO $ writeIORef vars currentVars
                   liftIO $ putStrLn $ "vars are: " ++ unwords currentVars
                   mainloop vars

main :: IO ()
main = do vars <- newIORef []
          runInputT (inputSettings vars) (runMaybeT (mainloop vars))
          return ()
