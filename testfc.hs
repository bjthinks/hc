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
  parseAll tokenizer "*"       ~?= Right [(0,TokenTimes),(1,TokenEnd)],
  parseAll tokenizer "%"       ~?= Right [(0,TokenPercent),(1,TokenEnd)]
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
  testEval "x*2" $ ExpressionProduct [ExpressionVariable "x",ExpressionInteger 2],
  testEval "x*(a+1)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionInteger 1]],
  testEval "x*(a+b)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(b+a)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+1+1)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionInteger 2]],
  testEval "x*(3+7+c+b+a+5+d+12)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c",ExpressionVariable "d",ExpressionInteger 27]],
  testEval "x*(a+b+0)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+b+0+0)" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+b+(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0)" $ ExpressionProduct[ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]],
  testEval "x*(a+(b+c))" $ ExpressionProduct [ExpressionVariable "x",ExpressionSum [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c"]],
  testEval "a*b+c*d" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],ExpressionProduct [ExpressionVariable "c",ExpressionVariable "d"]],
  testEval "x*y+z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],ExpressionVariable "z"],
  testEval "z+x*y" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],ExpressionVariable "z"],
  testEval "x*y+a" $ ExpressionSum [ExpressionVariable "a",ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"]],
  testEval "a+x*y" $ ExpressionSum [ExpressionVariable "a",ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"]],
  testEval "a*z+x" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "z"],ExpressionVariable "x"],
  testEval "x+a*z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "z"],ExpressionVariable "x"],
  testEval "a*b+c*2" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],ExpressionProduct [ExpressionVariable "c",ExpressionInteger 2]],
  testEval "c*2+a*b" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],ExpressionProduct [ExpressionVariable "c",ExpressionInteger 2]],
  testEval "a*c+b*2" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "c"],ExpressionProduct [ExpressionVariable "b",ExpressionInteger 2]],
  testEval "b*2+a*c" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "c"],ExpressionProduct [ExpressionVariable "b",ExpressionInteger 2]],
  testEval "b*c+a*2" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionInteger 2],ExpressionProduct [ExpressionVariable "b",ExpressionVariable "c"]],
  testEval "a*2+b*c" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionInteger 2],ExpressionProduct [ExpressionVariable "b",ExpressionVariable "c"]],
  testEval "x*z+y*z+x*y+x*y*z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y",ExpressionVariable "z"],
                                                ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],
                                                ExpressionProduct [ExpressionVariable "x",ExpressionVariable "z"],
                                                ExpressionProduct [ExpressionVariable "y",ExpressionVariable "z"]],
  testEval "z+y+x+x*z+y*z+x*y+x*y*z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y",ExpressionVariable "z"],
                                                      ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],
                                                      ExpressionProduct [ExpressionVariable "x",ExpressionVariable "z"],
                                                      ExpressionVariable "x",
                                                      ExpressionProduct [ExpressionVariable "y",ExpressionVariable "z"],
                                                      ExpressionVariable "y",ExpressionVariable "z"],
  testEval "1+z+y+x+x*z+y*z+x*y+x*y*z" $ ExpressionSum [ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y",ExpressionVariable "z"],
                                                        ExpressionProduct [ExpressionVariable "x",ExpressionVariable "y"],
                                                        ExpressionProduct [ExpressionVariable "x",ExpressionVariable "z"],
                                                        ExpressionVariable "x",
                                                        ExpressionProduct [ExpressionVariable "y",ExpressionVariable "z"],
                                                        ExpressionVariable "y",ExpressionVariable "z",ExpressionInteger 1],
  testEval "a*(y+1)+b*(x+1)" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionSum [ExpressionVariable "y",ExpressionInteger 1]],
                                              ExpressionProduct [ExpressionVariable "b",ExpressionSum [ExpressionVariable "x",ExpressionInteger 1]]],
  testEval "b*(x+1)+a*(y+1)" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionSum [ExpressionVariable "y",ExpressionInteger 1]],
                                              ExpressionProduct [ExpressionVariable "b",ExpressionSum [ExpressionVariable "x",ExpressionInteger 1]]],
  testEval "a*b+a*2" $ ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],
                                      ExpressionProduct [ExpressionVariable "a",ExpressionInteger 2]],
  testEval "a*(b*c)" $ ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b",ExpressionVariable "c"],
  testEval "a+b*(c*d)" $ ExpressionSum [ExpressionVariable "a",ExpressionProduct [ExpressionVariable "b",ExpressionVariable "c",ExpressionVariable "d"]],
  testEval "b*a" $ ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],
  testEval "a*(b+d*c)" $ ExpressionProduct [ExpressionVariable "a",
                                            ExpressionSum [ExpressionVariable "b",
                                                           ExpressionProduct [ExpressionVariable "c",
                                                                              ExpressionVariable "d"]]],
  testEval "a+b*(d+c)" $ ExpressionSum [ExpressionVariable "a",
                                        ExpressionProduct [ExpressionVariable "b",
                                                           ExpressionSum [ExpressionVariable "c",
                                                                          ExpressionVariable "d"]]],
  testEval "1*1" $ ExpressionInteger 1,
  testEval "1*1*1*(1*1*(1*(1*1)*1)*(1*1))*1" $ ExpressionInteger 1,
  testEval "(-1)*(-1)*(-1)*((-1)*(-1)*((-1)*((-1)*(-1))*(-1))*((-1)*(-1)))*(-1) " $ ExpressionInteger 1,
  testEval "a*(-1)*b*((-1)*c*((-1)*(d*(-1))*(-1))*((-1)*e))*(-1) " $ ExpressionProduct [ExpressionVariable "a", ExpressionVariable "b", ExpressionVariable "c", ExpressionVariable "d", ExpressionVariable "e", ExpressionInteger (-1)],
  testEval "2*3" $ ExpressionInteger 6,
  testEval "3*2" $ ExpressionInteger 6,
  testEval "1*x" $ ExpressionVariable "x",
  testEval "0*x" $ ExpressionInteger 0
  ]

expressionDisplayTests = [
  displayExpr (ExpressionInteger 3) ~?= "3",
  displayExpr (ExpressionInteger (-5)) ~?= "-5",
  displayExpr (ExpressionVariable "x") ~?= "x",
  displayExpr (ExpressionVariable "ABCdef123") ~?= "ABCdef123",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionInteger 1]) ~?= "a + 1",
  displayExpr (ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"]) ~?= "a + b",
  displayExpr (ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"]) ~?= "a b",
  -- This next one is a little bit bad, because the expression is not in standard form. But, we never expect display to change the form of an expression, so it ought to work.
  displayExpr (ExpressionProduct [ExpressionInteger 2,ExpressionVariable "b"]) ~?= "2 b",
  -- Product display should auto-move constants to front
  displayExpr (ExpressionProduct [ExpressionVariable "b",ExpressionInteger 2]) ~?= "2 b",
  displayExpr (ExpressionSum [ExpressionProduct [ExpressionVariable "a",ExpressionVariable "b"],ExpressionProduct [ExpressionInteger 2,ExpressionVariable "b"]]) ~?= "a b + 2 b",
  displayExpr (ExpressionProduct [ExpressionVariable "a",ExpressionSum [ExpressionVariable "b",ExpressionVariable "c"]]) ~?= "a (b + c)",
  displayExpr (ExpressionProduct [ExpressionSum [ExpressionVariable "a",ExpressionVariable "b"],ExpressionSum [ExpressionVariable "c",ExpressionVariable "d"]]) ~?= "(a + b) (c + d)",
  -- Also not in standard form (?)
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
