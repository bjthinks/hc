module Main where

import MyMaybeT
import Parser
import Tokenizer
import Expression
import Command
import CommandParser
import Store
import Data.Char
import Control.Exception as C
import HCException

import Data.List (isPrefixOf)
import Data.IORef
import System.Console.Haskeline

varsBeginningWith :: IORef Store -> String -> (MaybeT IO) [Completion]
varsBeginningWith s str = do
  store <- lift $ readIORef s
  let currentVars = getVariables store
  lift $ return $ map simpleCompletion $ filter (isPrefixOf str) currentVars

completeVars :: IORef Store -> CompletionFunc (MaybeT IO)
completeVars s =
  completeWord Nothing " \t\n\r" $ varsBeginningWith s

inputSettings :: IORef Store -> Settings (MaybeT IO)
inputSettings s = setComplete (completeVars s) defaultSettings

main :: IO ()
main = do putStrLn "Type control-c to interrupt lengthy computations."
          putStrLn "Note: assignments that form a loop may result in \"lengthy computations\"."
          s <- newIORef newStore
          runMaybeT (runInputT (inputSettings s) (mainloop s))
          -- putStrLn ""
          return ()

prompt :: String
prompt = "> "

mainloop :: IORef Store -> InputT (MaybeT IO) ()
mainloop s = do str <- liftIO $ getInputLine prompt
                store <- readIORef s
                store' <- liftIO ((processLine store str)
                                  `C.catch`
                                  (\e -> case e of
                                      UserInterrupt -> do putStrLn "\nCOMPUTATION INTERRUPTED! NOW YOU WILL NEVER KNOW THE ANSWER! MUAHAHAHAHA!"
                                                          return store
                                      _ -> C.throwIO e)
                                  `C.catch`
                                  (\e -> case e of
                                      HCDivideByZero -> do putStrLn "Y0U DIVIDED BY ZER0 S0 Y0U L0SE!"
                                                           return store
                                      HCNonIntegerPower -> do putStrLn "Input is well formed, but contains a noninteger power.\nOnly integer powers are currently supported."
                                                              return store))
                liftIO $ writeIORef s store'
                mainloop s

processLine :: Store -> String -> IO Store
processLine store str =
  case parseAll tokenizer str of
    Right [(_,TokenEnd)] -> return store
    Right tokens -> processTokens store tokens
    Left err -> do printError $ errorLocation err
                   return store

processTokens :: Store -> [(Int,Token)] -> IO Store
processTokens store tokens =
  case parseAll commandParser (map snd tokens) of
    Right cmd -> processCommand store cmd
    Left err -> let errorIndex = fst $ tokens !! errorLocation err in
      do printError errorIndex
         return store

processCommand :: Store -> Command -> IO Store
processCommand store cmd =
  let (store', output) = execute store cmd in
  do putStrLn output
     return store'

printError :: Int -> IO ()
printError d = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "ERROR: YOUR FAULT"
    where
      spaces = replicate (length prompt) ' '
      dashes = replicate d '-'
