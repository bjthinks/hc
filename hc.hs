module Main where

import Parser
import Tokenizer
import CommandParser
import Command
import Store
import HCException

import Data.List (isPrefixOf)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import System.Console.Haskeline
import qualified Control.Exception as E
import qualified System.Console.Haskeline.MonadException as ME
import qualified Control.Concurrent as C
import qualified System.Posix.Signals as S

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
  `E.catch` (putStrLn . hcErrorMessage)

printError :: Int -> IO ()
printError d = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Error: incomprehensible input"
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
  `E.catch` (\e -> case e of
                E.UserInterrupt -> putStrLn interruptMessage
                _ -> E.throwIO e)

-- get input

prompt :: String
prompt = "> "

getInputLineCatchingInterrupt :: InputT IO (Maybe String)
getInputLineCatchingInterrupt =
  getInputLine prompt
  `ME.catch` (\e -> case e of
                 E.UserInterrupt -> liftIO $ return $ Just ""
                 _ -> ME.throwIO e)

-- master control program

mainloop :: IORef Store -> MaybeT (InputT IO) ()
mainloop storeRef = do
  str <- MaybeT $ getInputLineCatchingInterrupt
  liftIO $ processLineCatchingInterrupt storeRef str
  mainloop storeRef

main :: IO ()
main = do
  -- Compensate for RTS brokenness
  tid <- C.myThreadId
  S.installHandler S.keyboardSignal
    (S.Catch (E.throwTo tid E.UserInterrupt)) Nothing
  -- Now we can proclaim proper control-C handling
  putStrLn "Type control-c to interrupt lengthy computations and control-d to exit."
  putStrLn "Note: assignments that form a loop may result in \"lengthy computations\"."
  -- Enter haskeline world
  s <- newIORef newStore
  runInputT (inputSettings s) (runMaybeT (mainloop s))
  return ()
