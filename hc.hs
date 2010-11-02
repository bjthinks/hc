import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import Command
import CommandParser
import Store
import Data.Char
import Control.Exception as C

main :: IO ()
main = do putStrLn "Don't type control-c at a prompt!  This will screw up the runtime environment."
          putStrLn "Do type control-c to interrupt lengthy computations."
          putStrLn "Note: assignments that form a loop may result in \"lengthy computations\"."
          setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT $ mainloop newStore
          putStrLn ""
          return ()

prompt :: String
prompt = "> "

mainloop :: Store -> MaybeT IO ()
mainloop store = do str <- MaybeT (readline prompt)
                    store' <- liftIO ((processLine store str)
                                      `C.catch`
                                      (\e -> case e of
                                          UserInterrupt -> do putStrLn "\nCOMPUTATION INTERRUPTED! NOW YOU WILL NEVER KNOW THE ANSWER! MUAHAHAHAHA!"
                                                              return store
                                          _ -> throwIO e)
                                      `C.catch`
                                      (\e -> case e of
                                          DivideByZero -> do putStrLn "Y0U DIVIDED BY ZER0 S0 Y0U L0SE!"
                                                             return store
                                          _ -> throwIO e))
                    liftIO $ setCompletionEntryFunction $ Just $
                      (return . makeCompletionFunction store')
                    mainloop store'

makeCompletionFunction :: Store -> String -> [String]
makeCompletionFunction store prefix =
  filter (startsWith prefix) $
  getVariables store

startsWith :: String -> String -> Bool
startsWith prefix str = take (length prefix) str == prefix

processLine :: Store -> String -> IO Store
processLine store str =
  case parseAll tokenizer str of
    Right [(_,TokenEnd)] -> return store
    Right tokens -> do addHistory str
                       processTokens store tokens
    Left err -> do addHistory str
                   printError $ errorLocation err
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
