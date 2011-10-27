module Main where

import Parser
import Tokenizer
import Expression
import Command
import CommandParser
import Store
import Data.Char
import Control.Exception as C
import HCException

import Control.Monad.Trans.Maybe
import Data.List (isPrefixOf)
import Data.IORef
import System.Console.Haskeline
import Control.Monad.IO.Class

-- inputSettings

varsBeginningWith :: IORef Store -> String -> IO [Completion]
varsBeginningWith s str = do
  store <- readIORef s
  let currentVars = getVariables store
  return $ map simpleCompletion $ filter (isPrefixOf str) currentVars

completeVars :: IORef Store -> CompletionFunc IO
completeVars s =
  completeWord Nothing " \t\n\r" $ varsBeginningWith s

inputSettings :: IORef Store -> Settings IO
inputSettings s = setComplete (completeVars s) defaultSettings

-- mainloop

prompt :: String
prompt = "> "

processCommand :: Store -> Command -> IO Store
processCommand store cmd =
  let (store', output) = execute store cmd in
  do putStrLn output
     return store'
  `C.catch` (\e -> putStrLn (hcErrorMessage e) >> return store)

printError :: Int -> IO ()
printError d = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "ERROR: YOUR FAULT"
    where
      spaces = replicate (length prompt) ' '
      dashes = replicate d '-'

processTokens :: Store -> [(Int,Token)] -> IO Store
processTokens store tokens =
  case parseAll commandParser (map snd tokens) of
    Right cmd -> processCommand store cmd
    Left err -> let errorIndex = fst $ tokens !! errorLocation err in
      do printError errorIndex
         return store

processLine :: Store -> String -> IO Store
processLine store str =
  case parseAll tokenizer str of
    Right [(_,TokenEnd)] -> return store
    Right tokens -> processTokens store tokens
    Left err -> do printError $ errorLocation err
                   return store

mainloop :: IORef Store -> MaybeT (InputT IO) ()
mainloop s = do str <- MaybeT $ getInputLine prompt
                store <- liftIO $ readIORef s
                store' <- liftIO $ processLine store str
                liftIO $ writeIORef s store'
                mainloop s
{-
UserInterrupt -> do putStrLn "\nCOMPUTATION INTERRUPTED! NOW YOU WILL NEVER KNOW THE ANSWER! MUAHAHAHAHA!"
                    return store
-}

main :: IO ()
main = do putStrLn "Type control-c to interrupt lengthy computations."
          putStrLn "Note: assignments that form a loop may result in \"lengthy computations\"."
          s <- newIORef newStore
          runInputT (inputSettings s) (runMaybeT (mainloop s))
          -- putStrLn ""
          return ()
