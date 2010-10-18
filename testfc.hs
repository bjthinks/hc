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

(tInt,tVar,tSum,tProd) = useThisVariableOnlyForTestingTheExpressionConstructors

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
  testEval "4" (tInt 4),
  testEval "-8" (tInt (-8)),
  testEval "foo" $ tVar "foo",
  testEval "%3" $ tVar "%3",
  testEval "a+1" $ tSum [tVar "a",tInt 1],
  testEval "a+b" $ tSum [tVar "a",tVar "b"],
  testEval "b+a" $ tSum [tVar "a",tVar "b"],
  testEval "a+1+1" $ tSum [tVar "a",tInt 2],
  testEval "3+7+c+b+a+5+d+12" $ tSum [tVar "a",tVar "b",tVar "c",tVar "d",tInt 27],
  testEval "a+0" $ tVar "a",
  testEval "0" $ tInt 0,
  testEval "0+0" $ tInt 0,
  testEval "(0)" $ tInt 0,
  testEval "(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0" $ tInt 0,
  testEval "(1+1)+(1+(1+1)+(1+1+(1+1))+1)+1" $ tInt 11,
  testEval "(x+1)+y" $ tSum [tVar "x",tVar "y",tInt 1],
  testEval "(x+1)+y+1" $ tSum [tVar "x",tVar "y",tInt 2],
  testEval "x*y" $ tProd [tVar "x",tVar "y"],
  testEval "x*2" $ tProd [tVar "x",tInt 2],
  testEval "x*(a+1)" $ tProd [tVar "x",tSum [tVar "a",tInt 1]],
  testEval "x*(a+b)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(b+a)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(a+1+1)" $ tProd [tVar "x",tSum [tVar "a",tInt 2]],
  testEval "x*(3+7+c+b+a+5+d+12)" $ tProd [tVar "x",tSum [tVar "a",tVar "b",tVar "c",tVar "d",tInt 27]],
  testEval "x*(a+b+0)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(a+b+0+0)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(a+b+(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0)" $ tProd[tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(a+(b+c))" $ tProd [tVar "x",tSum [tVar "a",tVar "b",tVar "c"]],
  testEval "a*b+c*d" $ tSum [tProd [tVar "a",tVar "b"],tProd [tVar "c",tVar "d"]],
  testEval "x*y+z" $ tSum [tProd [tVar "x",tVar "y"],tVar "z"],
  testEval "z+x*y" $ tSum [tProd [tVar "x",tVar "y"],tVar "z"],
  testEval "x*y+a" $ tSum [tVar "a",tProd [tVar "x",tVar "y"]],
  testEval "a+x*y" $ tSum [tVar "a",tProd [tVar "x",tVar "y"]],
  testEval "a*z+x" $ tSum [tProd [tVar "a",tVar "z"],tVar "x"],
  testEval "x+a*z" $ tSum [tProd [tVar "a",tVar "z"],tVar "x"],
  testEval "a*b+c*2" $ tSum [tProd [tVar "a",tVar "b"],tProd [tVar "c",tInt 2]],
  testEval "c*2+a*b" $ tSum [tProd [tVar "a",tVar "b"],tProd [tVar "c",tInt 2]],
  testEval "a*c+b*2" $ tSum [tProd [tVar "a",tVar "c"],tProd [tVar "b",tInt 2]],
  testEval "b*2+a*c" $ tSum [tProd [tVar "a",tVar "c"],tProd [tVar "b",tInt 2]],
  testEval "b*c+a*2" $ tSum [tProd [tVar "a",tInt 2],tProd [tVar "b",tVar "c"]],
  testEval "a*2+b*c" $ tSum [tProd [tVar "a",tInt 2],tProd [tVar "b",tVar "c"]],
  testEval "x*z+y*z+x*y+x*y*z" $ tSum [tProd [tVar "x",tVar "y",tVar "z"],
                                                tProd [tVar "x",tVar "y"],
                                                tProd [tVar "x",tVar "z"],
                                                tProd [tVar "y",tVar "z"]],
  testEval "z+y+x+x*z+y*z+x*y+x*y*z" $ tSum [tProd [tVar "x",tVar "y",tVar "z"],
                                                      tProd [tVar "x",tVar "y"],
                                                      tProd [tVar "x",tVar "z"],
                                                      tVar "x",
                                                      tProd [tVar "y",tVar "z"],
                                                      tVar "y",tVar "z"],
  testEval "1+z+y+x+x*z+y*z+x*y+x*y*z" $ tSum [tProd [tVar "x",tVar "y",tVar "z"],
                                                        tProd [tVar "x",tVar "y"],
                                                        tProd [tVar "x",tVar "z"],
                                                        tVar "x",
                                                        tProd [tVar "y",tVar "z"],
                                                        tVar "y",tVar "z",tInt 1],
  testEval "a*(y+1)+b*(x+1)" $ tSum [tProd [tVar "a",tSum [tVar "y",tInt 1]],
                                              tProd [tVar "b",tSum [tVar "x",tInt 1]]],
  testEval "b*(x+1)+a*(y+1)" $ tSum [tProd [tVar "a",tSum [tVar "y",tInt 1]],
                                              tProd [tVar "b",tSum [tVar "x",tInt 1]]],
  testEval "a*b+a*2" $ tSum [tProd [tVar "a",tVar "b"],
                                      tProd [tVar "a",tInt 2]],
  testEval "a*(b*c)" $ tProd [tVar "a",tVar "b",tVar "c"],
  testEval "a+b*(c*d)" $ tSum [tVar "a",tProd [tVar "b",tVar "c",tVar "d"]],
  testEval "b*a" $ tProd [tVar "a",tVar "b"],
  testEval "a*(b+d*c)" $ tProd [tVar "a",
                                            tSum [tVar "b",
                                                           tProd [tVar "c",
                                                                              tVar "d"]]],
  testEval "a+b*(d+c)" $ tSum [tVar "a",
                                        tProd [tVar "b",
                                                           tSum [tVar "c",
                                                                          tVar "d"]]],
  testEval "1*1" $ tInt 1,
  testEval "1*1*1*(1*1*(1*(1*1)*1)*(1*1))*1" $ tInt 1,
  testEval "(-1)*(-1)*(-1)*((-1)*(-1)*((-1)*((-1)*(-1))*(-1))*((-1)*(-1)))*(-1) " $ tInt 1,
  testEval "a*(-1)*b*((-1)*c*((-1)*(d*(-1))*(-1))*((-1)*e))*(-1) " $ tProd [tVar "a", tVar "b", tVar "c", tVar "d", tVar "e", tInt (-1)],
  testEval "2*3" $ tInt 6,
  testEval "3*2" $ tInt 6,
  testEval "1*x" $ tVar "x",
  testEval "0*x" $ tInt 0,
  testEval "a+a" $ tProd [tVar "a",tInt 2],
  testEval "a+2*a" $ tProd [tVar "a",tInt 3],
  testEval "2*a+2*a" $ tProd [tVar "a",tInt 4],
  testEval "3*a+5*a" $ tProd [tVar "a",tInt 8],
  testEval "c+a+b+c+b+a+b+b+c+c+c+a+b+a+c" $ tSum [tProd [tVar "a",tInt 4],
                                                            tProd [tVar "b",tInt 5],
                                                            tProd [tVar "c",tInt 6]],
  testEval "2*c+5*a+7*b+6*c+3*b+9*a+4*b+5*b+3*c+8*c+7*c+2*a+4*b+6*a+6*c" $
  tSum [tProd [tVar "a",tInt 22],
                 tProd [tVar "b",tInt 23],
                 tProd [tVar "c",tInt 32]],
  testEval "(a+1)*(b+2)*c*(d+3)+(d+3)*5*(b+2)*c*(a+1)+4*(d+3)*c*(b+2)*(a+1)" $
  tProd [tVar "c",
                     tSum [tVar "a",tInt 1],
                     tSum [tVar "b",tInt 2],
                     tSum [tVar "d",tInt 3],
                     tInt 10]
  ]

expressionDisplayTests = [
  displayExpr (tInt 3) ~?= "3",
  displayExpr (tInt (-5)) ~?= "-5",
  displayExpr (tVar "x") ~?= "x",
  displayExpr (tVar "ABCdef123") ~?= "ABCdef123",
  displayExpr (tSum [tVar "a",tInt 1]) ~?= "a + 1",
  displayExpr (tSum [tVar "a",tVar "b"]) ~?= "a + b",
  displayExpr (tProd [tVar "a",tVar "b"]) ~?= "a b",
  -- This next one is a little bit bad, because the expression is not in standard form. But, we never expect display to change the form of an expression, so it ought to work.
  displayExpr (tProd [tInt 2,tVar "b"]) ~?= "2 b",
  -- Product display should auto-move constants to front
  displayExpr (tProd [tVar "b",tInt 2]) ~?= "2 b",
  displayExpr (tSum [tProd [tVar "a",tVar "b"],tProd [tInt 2,tVar "b"]]) ~?= "a b + 2 b",
  displayExpr (tProd [tVar "a",tSum [tVar "b",tVar "c"]]) ~?= "a (b + c)",
  displayExpr (tProd [tSum [tVar "a",tVar "b"],tSum [tVar "c",tVar "d"]]) ~?= "(a + b) (c + d)",
  -- Also not in standard form (?)
  displayExpr (tProd [tSum [tVar "a",tVar "b"],tVar "c"]) ~?= "(a + b) c",
  displayExpr (tProd [tSum [tVar "a",tVar "b"],tSum [tVar "c",tVar "d"],tSum [tVar "e",tInt 1]]) ~?= "(a + b) (c + d) (e + 1)"
  ]

storeTests = [
  getValue "a" newStore ~?= Nothing,
  getValue "a" (setValue "a" (tInt 42) newStore) ~?= Just (tInt 42),
  getValue "b" (setValue "a" (tInt 42) newStore) ~?= Nothing,
  getValue "a" (setValue "a" (tVar "x") (setValue "a" (tInt 11) newStore)) ~?= Just (tVar "x"),
  getValue "a" (setValue "b" (tVar "x") (setValue "a" (tInt 11) newStore)) ~?= Just (tInt 11),
  getValue "a" (setValue "a" (tVar "x") (setValue "b" (tInt 11) newStore)) ~?= Just (tVar "x")
  ]

tests = test (map ("tokenizer" ~:) tokenizerTests ++
              map ("expression parser" ~:) expressionParserTests ++
              map ("expression display" ~:) expressionDisplayTests ++
              map ("store" ~:) storeTests)

main = runTestTT tests
