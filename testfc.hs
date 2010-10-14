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
  parseAll tokenizer ""        ~?= Right [(0,TokenEnd)],
  parseAll tokenizer "1"       ~?= Right [(0,TokenInteger 1),(1,TokenEnd)],
  parseAll tokenizer "123"     ~?= Right [(0,TokenInteger 123),(3,TokenEnd)],
  parseAll tokenizer " 1"      ~?= Right [(1,TokenInteger 1),(2,TokenEnd)],
  parseAll tokenizer "   456"  ~?= Right [(3,TokenInteger 456),(6,TokenEnd)],
  parseAll tokenizer " 1 2 3 " ~?= Right [(1,TokenInteger 1),(3,TokenInteger 2),(5,TokenInteger 3),(7,TokenEnd)],
  parseAll tokenizer "    345    5678   34    " ~?= Right [(4,TokenInteger 345),(11,TokenInteger 5678),(18,TokenInteger 34),(24,TokenEnd)],
  parseAll tokenizer "-"       ~?= Right [(0,TokenMinus),(1,TokenEnd)],
  parseAll tokenizer " - 9 "   ~?= Right [(1,TokenMinus),(3,TokenInteger 9),(5,TokenEnd)],
  parseAll tokenizer "---"     ~?= Right [(0,TokenMinus),(1,TokenMinus),(2,TokenMinus),(3,TokenEnd)],
  parseAll tokenizer "abc"     ~?= Right [(0,TokenWord "abc"),(3,TokenEnd)],
  parseAll tokenizer "ABC"     ~?= Right [(0,TokenWord "ABC"),(3,TokenEnd)],
  parseAll tokenizer "a23"     ~?= Right [(0,TokenWord "a23"),(3,TokenEnd)],
  parseAll tokenizer " a11 11" ~?= Right [(1,TokenWord "a11"),(5,TokenInteger 11),(7,TokenEnd)],
  parseAll tokenizer ":="      ~?= Right [(0,TokenAssign),(2,TokenEnd)],
  parseAll tokenizer "3-:=-3"  ~?= Right [(0,TokenInteger 3),(1,TokenMinus),(2,TokenAssign),(4,TokenMinus),(5,TokenInteger 3),(6,TokenEnd)],
  parseAll tokenizer "+"       ~?= Right [(0,TokenPlus),(1,TokenEnd)]
  ]

unRight :: Either a b -> b
unRight (Right b) = b
unRight _ = error "tokenizer failed"

testEval str expr = parseAll expressionParser (map snd (unRight (parseAll tokenizer str))) ~?= Right expr

expressionParserTests = [
  testEval "4" (ExpressionInteger 4),
  testEval "-8" (ExpressionInteger (-8)),
  testEval "foo" $ ExpressionVariable "foo",
  testEval "a+1" $ ExpressionSum [ExpressionVariable "a",ExpressionInteger 1],
  testEval "a+b" $ ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]
--  testEval "b+a" $ ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]
  ]

expressionDisplayTests = [
  displayExpr (ExpressionInteger 3) ~?= "3",
  displayExpr (ExpressionInteger (-5)) ~?= "-5",
  displayExpr (ExpressionVariable "x") ~?= "x",
  displayExpr (ExpressionVariable "ABCdef123") ~?= "ABCdef123",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionInteger 1]) ~?= "a + 1",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]) ~?= "a + b"
  ]

storeTests = [
  getValue "a" newStore ~?= Nothing,
  getValue "a" (setValue "a" (ExpressionInteger 42) newStore) ~?= Just (ExpressionInteger 42),
  getValue "b" (setValue "a" (ExpressionInteger 42) newStore) ~?= Nothing,
  getValue "a" (setValue "a" (ExpressionVariable "x") (setValue "a" (ExpressionInteger 11) newStore)) ~?= Just (ExpressionVariable "x"),
  getValue "a" (setValue "b" (ExpressionVariable "x") (setValue "a" (ExpressionInteger 11) newStore)) ~?= Just (ExpressionInteger 11),
  getValue "a" (setValue "a" (ExpressionVariable "x") (setValue "b" (ExpressionInteger 11) newStore)) ~?= Just (ExpressionVariable "x")
  ]

tests = test (map ("tokenizer" ~:) tokenizerTests ++
              map ("expression parser" ~:) expressionParserTests ++
              map ("expression display" ~:) expressionDisplayTests ++
              map ("store" ~:) storeTests)

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
