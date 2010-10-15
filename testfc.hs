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
  parseAll tokenizer "+"       ~?= Right [(0,TokenPlus),(1,TokenEnd)],
  parseAll tokenizer "("       ~?= Right [(0,TokenOpenParen),(1,TokenEnd)],
  parseAll tokenizer ")"       ~?= Right [(0,TokenCloseParen),(1,TokenEnd)],
  parseAll tokenizer "*"       ~?= Right [(0,TokenTimes),(1,TokenEnd)]
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
  testEval "a+b" $ ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],
  testEval "b+a" $ ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],
  testEval "a+1+1" $ ExpressionSum [ExpressionVariable "a",ExpressionInteger 2],
  testEval "3+7+c+b+a+5+d+12" $ ExpressionSum [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c",ExpressionVariable "d",ExpressionInteger 27],
  testEval "a+0" $ ExpressionVariable "a",
  testEval "0" $ ExpressionInteger 0,
  testEval "0+0" $ ExpressionInteger 0,
  testEval "(0)" $ ExpressionInteger 0,
  testEval "(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0" $ ExpressionInteger 0,
  testEval "(1+1)+(1+(1+1)+(1+1+(1+1))+1)+1" $ ExpressionInteger 11,
  testEval "(x+1)+y" $ ExpressionSum [ExpressionVariable "x",ExpressionVariable "y",ExpressionInteger 1],
  testEval "(x+1)+y+1" $ ExpressionSum [ExpressionVariable "x",ExpressionVariable "y",ExpressionInteger 2],
  testEval "x*y" $ ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],
  testEval "2*x" $ ExpressionProduct [ExpressionInteger 2,ExpressionVariable "x"],
  testEval "x*(a+1)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionInteger 1]],
  testEval "x*(a+b)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(b+a)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+1+1)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionInteger 2]],
  testEval "x*(3+7+c+b+a+5+d+12)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c",ExpressionVariable "d",ExpressionInteger 27]],
  testEval "x*(a+b+0)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+b+0+0)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+b+(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0)" $ ExpressionProduct[ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+(b+c))" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c"]]
  --testEval "x*y+z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],ExpressionVariable "z"],
  --testEval "z+x*y" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],ExpressionVariable "z"],
  --testEval "x*y+a" $ ExpressionSum [ExpressionVariable "a",ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"]],
  --testEval "a+x*y" $ ExpressionSum [ExpressionVariable "a",ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"]],
  --testEval "a*z+x" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "z"],ExpressionVariable "x"],
  --testEval "x+a*z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "z"],ExpressionVariable "x"]
  ]

expressionDisplayTests = [
  displayExpr (ExpressionInteger 3) ~?= "3",
  displayExpr (ExpressionInteger (-5)) ~?= "-5",
  displayExpr (ExpressionVariable "x") ~?= "x",
  displayExpr (ExpressionVariable "ABCdef123") ~?= "ABCdef123",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionInteger 1]) ~?= "a + 1",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]) ~?= "a + b",
  displayExpr (ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"]) ~?= "a b",
  displayExpr (ExpressionProduct [ExpressionInteger 2,ExpressionVariable "b"]) ~?= "2 b",
  displayExpr (ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],ExpressionProduct [ExpressionInteger 2,ExpressionVariable "b"]]) ~?= "a b + 2 b",
  displayExpr (ExpressionProduct [ExpressionVariable "a",ExpressionSum [ExpressionVariable "b",ExpressionVariable "c"]]) ~?= "a (b + c)",
  displayExpr (ExpressionProduct [ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],ExpressionSum [ExpressionVariable "c",ExpressionVariable "d"]]) ~?= "(a + b) (c + d)",
  -- This next one is a little bit bad, because the expression is not in standard form. But, we never expect display to change the form of an expression, so it ought to work.
  displayExpr (ExpressionProduct [ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],ExpressionVariable "c"]) ~?= "(a + b) c",
  displayExpr (ExpressionProduct [ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],ExpressionSum [ExpressionVariable "c",ExpressionVariable "d"],ExpressionSum [ExpressionVariable "e",ExpressionInteger 1]]) ~?= "(a + b) (c + d) (e + 1)"
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
