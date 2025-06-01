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

-- tab completion

varsBeginningWith :: IORef Store -> String -> IO [Completion]
varsBeginningWith s str = do
  store <- readIORef s
  let currentVars = getVariables store ++ (map (++"(") builtinFunctions) ++
        builtinCommands
  return $ map simpleCompletion $ filter (isPrefixOf str) currentVars

completeVars :: IORef Store -> CompletionFunc IO
completeVars s =
  completeWord Nothing " \t\n\r" $ varsBeginningWith s

inputSettings :: IORef Store -> Settings IO
inputSettings s = setComplete (completeVars s) defaultSettings

-- handle input

processCommand :: IORef Store -> Command -> IO Bool
processCommand storeRef cmd =
  do store <- readIORef storeRef
     let (store', output) = execute store cmd
     putStrLn output
     writeIORef storeRef store'
     return True
  `E.catch` (\e -> do
     putStrLn $ hcErrorMessage e
     return False)

processCommands :: IORef Store -> [Command] -> IO ()
processCommands _ [] = return ()
processCommands storeRef (c:cs) = do
  continue <- processCommand storeRef c
  if continue
    then processCommands storeRef cs
    else return ()

printError :: Int -> IO ()
printError d = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Please check your input and try again."
    where
      spaces = replicate (length prompt) ' '
      dashes = replicate d '-'

processTokens :: IORef Store -> [(Int,Token)] -> IO ()
processTokens storeRef tokens =
  case parseAll commandParser (map snd tokens) of
    Right cmds -> processCommands storeRef cmds
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
  handleInterrupt (return $ Just "") $ withInterrupt $ getInputLine prompt

-- master control program

mainloop :: IORef Store -> MaybeT (InputT IO) ()
mainloop storeRef = do
  str <- MaybeT $ getInputLineCatchingInterrupt
  liftIO $ processLineCatchingInterrupt storeRef str
  mainloop storeRef

main :: IO ()
main = do
  sequence_ $ map putStrLn banner
  -- Enter haskeline world
  s <- newIORef newStore
  _ <- runInputT (inputSettings s) (runMaybeT (mainloop s)) `E.catch`
         (\e -> case e of HCExit -> return Nothing)
  return ()

-- The ASCII art is shamelessly copied from:
-- https://patorjk.com/software/taag/#p=display&f=Isometric1&t=hc
banner :: [String]
banner =
  [ "      ___           ___     "
  , "     /\\__\\         /\\  \\    "
  , "    /:/  /        /::\\  \\   "
  , "   /:/__/        /:/\\:\\  \\  "
  , "  /::\\  \\ ___   /:/  \\:\\  \\ "
  , " /:/\\:\\  /\\__\\ /:/__/ \\:\\__\\" ++
    "  hc: A calculator for rational functions."
  , " \\/__\\:\\/:/  / \\:\\  \\  \\/__/"
  , "      \\::/  /   \\:\\  \\      "
  , "      /:/  /     \\:\\  \\     "
  , "     /:/  /       \\:\\__\\    "
  , "     \\/__/         \\/__/    "
  , "hc can do arbitrary precision integer and rational number arithmetic, and"
  , "calculations with polynomials and rational functions. Please enter an"
  , "algebraic expression using numbers, variables, and + - * /. You can also"
  , "raise anything to a whole number power using ^. Assign to variables and"
  , "functions using the := operator. Type help for more information."
  , "Use control-c to interrupt lengthy computations."
  , "Note: assignments that form a loop will cause the interpreter to hang."
  ]
