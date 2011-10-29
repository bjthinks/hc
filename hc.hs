module Main where

import Parser
import Tokenizer
import Expression
import Command
import CommandParser
import Store
import HCException

import Data.Char
import Control.Exception as C
import Control.Exception.Base
import Control.Monad.Trans.Maybe
import Data.List (isPrefixOf)
import Data.IORef
import System.Console.Haskeline as HL
import System.Console.Haskeline.MonadException as ME
import Control.Monad.IO.Class

-- tab completion

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

-- handle input

processCommand :: IORef Store -> Command -> IO ()
processCommand storeRef cmd =
  do store <- readIORef storeRef
     let (store', output) = execute store cmd
     putStrLn output
     writeIORef storeRef store'
  `C.catch` (putStrLn . hcErrorMessage)

printError :: Int -> IO ()
printError d = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Error: "
    where
      spaces = replicate (length prompt) ' '
      dashes = replicate d '-'

processTokens :: IORef Store -> [(Int,Token)] -> IO ()
processTokens storeRef tokens =
  case parseAll commandParser (map snd tokens) of
    Right cmd -> processCommand storeRef cmd
    Left err -> let errorIndex = fst $ tokens !! errorLocation err
                in printError errorIndex

processLine :: IORef Store -> String -> IO ()
processLine storeRef str =
  case parseAll tokenizer str of
    Right [(_,TokenEnd)] -> return ()
    Right tokens -> processTokens storeRef tokens
    Left err -> printError $ errorLocation err

interruptMessage :: String
interruptMessage = "\nInterrupted"

processLineCatchingInterrupt :: IORef Store -> String -> IO ()
processLineCatchingInterrupt storeRef str =
  processLine storeRef str
  `C.catch` (\e -> case e of
                UserInterrupt -> putStrLn interruptMessage
                _ -> C.throwIO e)

-- get input

prompt :: String
prompt = "> "

getInputLineCatchingInterrupt :: InputT IO (Maybe String)
getInputLineCatchingInterrupt =
  getInputLine prompt
  `ME.catch` (\e -> case e of
                 UserInterrupt -> liftIO $ return $ Just ""
                 _ -> ME.throwIO e)

-- master control program

mainloop :: IORef Store -> MaybeT (InputT IO) ()
mainloop storeRef = do
  str <- MaybeT $ getInputLineCatchingInterrupt
  liftIO $ processLineCatchingInterrupt storeRef str
  mainloop storeRef

main :: IO ()
main = do putStrLn "Type control-c to interrupt lengthy computations and control-d to exit."
          putStrLn "Note: assignments that form a loop may result in \"lengthy computations\"."
          s <- newIORef newStore
          runInputT (inputSettings s) (runMaybeT (mainloop s))
          return ()
