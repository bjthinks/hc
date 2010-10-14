import Test.HUnit

import MyMaybeT
import Parser
import Tokenizer
import Expression
import ExpressionParser
import ExpressionDisplay
import Command
import CommandParser
import Store

tokenizerTests = [
  parseAll tokenizer "" ~?= Right []
  ]

tests = test (tokenizerTests)

main = runTestTT tests

{-
This is just junk that I copied and pasted from the parser project.
Don't know if I'll use it.

isFail :: Either a b -> Bool
isFail (Left _) = True
isFail (Right _) = False

testPart :: (Show a, Eq a) => Parser Char a -> String -> a -> String -> Test
testPart parser input output rest
  = test $ parseSome parser input ~?= Right
    (output, length input - length rest, rest)

testSucc :: (Show a, Eq a) => Parser Char a -> String -> a -> Test
testSucc parser input output
  = testPart parser input output ""

testFail :: (Show t) => Parser t a -> [t] -> Test
testFail parser input
  = test $ unless (isFail $ parseSome parser input)
    (assertFailure "Expected failure, but succeeded")

testEquiv :: (Eq a, Eq t) => Parser t a -> Parser t a -> [[t]] -> Test
testEquiv parser1 parser2 inputs
  = TestCase $ unless
    (map (parseSome parser1) inputs == map (parseSome parser2) inputs)
    (assertFailure "Inequivalent parsers")

testError :: (Eq a, Eq t) => Parser t a -> [t] -> Int -> [(Int, String)] ->
             Test
testError parser input error_position names
  = test $ unless
    ((actual_position, actual_names) == (error_position, names))
    (assertFailure $ "Wrong error, expected "
     ++ show (error_position, names) ++ " but got "
     ++ show (actual_position, actual_names))
      where
        Left error_result = parseSome parser input
        actual_position = errorLocation error_result
        actual_names = errorNames error_result
-}
