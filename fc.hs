import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser
import ExpressionDisplay
import Command
import CommandParser
import Store

main :: IO ()
main = do putStrLn "Don't type control-c"
          putStrLn "Don't make assignments that form a loop"
          setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT $ mainloop (setValue "#" (ExpressionInteger 0) newStore)
          putStrLn ""
          return ()

prompt :: String
prompt = "> "

mainloop :: Store -> MaybeT IO ()
mainloop store = do str <- MaybeT (readline prompt)
                    store' <- liftIO $ processLine store str
                    mainloop store'

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
