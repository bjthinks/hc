module Factor.Main where

import Factor.Tests

main :: IO ()
main = do
  summary <- runTests
  print summary
