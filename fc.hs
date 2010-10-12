import System.Console.Readline

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser
import ExpressionDisplay
import qualified Data.Map as Map

main :: IO ()
main = do putStrLn "Don't type control-c"
          setCompletionEntryFunction $ Just $ \_ -> return []
          runMaybeT $ mainloop newStore
          putStrLn ""
          return ()

prompt :: String
prompt = "> "

type Store = Map.Map String Expression

newStore :: Store
newStore = Map.empty

mainloop :: Store -> MaybeT IO ()
mainloop store = do str <- MaybeT (readline prompt)
                    store' <- liftIO $ processLine store str
                    mainloop store'

processLine :: Store -> String -> IO Store
processLine store str =
  case parseAll tokenizer str of
    Right [] -> return store
    Right tokens -> do addHistory str
                       processTokens store tokens
    Left err -> do addHistory str
                   printError (length prompt) (errorLocation err)
                     "unrecognized input"
                   return store

processTokens :: Store -> [(Int,Token)] -> IO Store
processTokens store tokens =
  case parseAll commandParser (map snd tokens) of
    Right cmd -> do store' <- printResult store cmd
                    return store'
    Left err -> let stringLocation = fst $ tokens !! errorLocation err in
      do printError (length prompt) stringLocation "unrecognized expression"
         return store

printResult :: Store -> Command -> IO Store
printResult store cmd = do putStrLn $ display cmd
                           return store

printError :: Int -> Int -> String -> IO ()
printError s d m = do
  putStrLn $ spaces ++ dashes ++ "^"
  putStrLn $ "Error: " ++ m
    where
      spaces = replicate s ' '
      dashes = replicate d '-'
