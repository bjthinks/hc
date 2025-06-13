import Test.HUnit

import Parser
import Tokenizer
import ASTParser
import Command
import CommandParser
import Expression
import ExprFromAST
import ASTFromExpr
import ASTDisplay
import Store
import Expand
import Together
import Factor

tRat :: Rational -> Expression
tVar :: String -> Expression
tSum :: [Expression] -> Expression
tProd :: [Expression] -> Expression
tIntPow :: Expression -> Integer -> Expression
tCall :: String -> [Expression] -> Expression
(tRat,tVar,tSum,tProd,tIntPow,tCall) = unsafeExpressionConstructors

unRight :: Either a b -> b
unRight (Right b) = b
unRight _ = error "parse failed"

expressionParser :: Parser Token Expression
expressionParser = do
  ast <- astParser
  _ <- match TokenEnd
  return (fromAST ast)

testEval :: String -> Expression -> Test
testEval str expr = parseAll expressionParser (map snd (unRight (parseAll tokenizer str))) ~?= Right expr

expressionParserTests :: Test
expressionParserTests = test [
  testEval "4" (tRat 4),
  testEval "-8" (tRat (-8)),
  testEval "foo" $ tVar "foo",
  testEval "a+1" $ tSum [tVar "a",tRat 1],
  testEval "a+b" $ tSum [tVar "a",tVar "b"],
  testEval "b+a" $ tSum [tVar "a",tVar "b"],
  testEval "a+1+1" $ tSum [tVar "a",tRat 2],
  testEval "3+7+c+b+a+5+d+12" $ tSum [tVar "a",tVar "b",tVar "c",tVar "d",tRat 27],
  testEval "a+0" $ tVar "a",
  testEval "0" $ tRat 0,
  testEval "0+0" $ tRat 0,
  testEval "(0)" $ tRat 0,
  testEval "(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0" $ tRat 0,
  testEval "(1+1)+(1+(1+1)+(1+1+(1+1))+1)+1" $ tRat 11,
  testEval "(x+1)+y" $ tSum [tVar "x",tVar "y",tRat 1],
  testEval "(x+1)+y+1" $ tSum [tVar "x",tVar "y",tRat 2],
  testEval "x*y" $ tProd [tVar "x",tVar "y"],
  testEval "x*2" $ tProd [tVar "x",tRat 2],
  testEval "x*(a+1)" $ tProd [tVar "x",tSum [tVar "a",tRat 1]],
  testEval "x*(a+b)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(b+a)" $ tProd [tVar "x",tSum [tVar "a",tVar "b"]],
  testEval "x*(a+1+1)" $ tProd [tVar "x",tSum [tVar "a",tRat 2]],
  testEval "x*(3+7+c+b+a+5+d+12)" $ tProd [tVar "x",tSum [tVar "a",tVar "b",tVar "c",tVar "d",tRat 27]],
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
  testEval "a*b+2*c" $ tSum [tProd [tVar "a",tVar "b"],tProd [tVar "c",tRat 2]],
  testEval "c*2+a*b" $ tSum [tProd [tVar "a",tVar "b"],tProd [tVar "c",tRat 2]],
  testEval "a*c+2*b" $ tSum [tProd [tVar "a",tVar "c"],tProd [tVar "b",tRat 2]],
  testEval "b*2+a*c" $ tSum [tProd [tVar "a",tVar "c"],tProd [tVar "b",tRat 2]],
  testEval "b*c+a*2" $ tSum [tProd [tVar "a",tRat 2],tProd [tVar "b",tVar "c"]],
  testEval "2*a+b*c" $ tSum [tProd [tVar "a",tRat 2],tProd [tVar "b",tVar "c"]],
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
                                               tVar "y",tVar "z",tRat 1],
  testEval "a*(y+1)+b*(x+1)" $ tSum [tProd [tVar "a",tSum [tVar "y",tRat 1]],
                                     tProd [tVar "b",tSum [tVar "x",tRat 1]]],
  testEval "b*(x+1)+a*(y+1)" $ tSum [tProd [tVar "a",tSum [tVar "y",tRat 1]],
                                     tProd [tVar "b",tSum [tVar "x",tRat 1]]],
  testEval "a*b+a*2" $ tSum [tProd [tVar "a",tVar "b"],
                             tProd [tVar "a",tRat 2]],
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
  testEval "1*1" $ tRat 1,
  testEval "1*1*1*(1*1*(1*(1*1)*1)*(1*1))*1" $ tRat 1,
  testEval "(-1)*(-1)*(-1)*((-1)*(-1)*((-1)*((-1)*(-1))*(-1))*((-1)*(-1)))*(-1) " $ tRat 1,
  testEval "a*(-1)*b*((-1)*c*((-1)*(d*(-1))*(-1))*((-1)*e))*(-1) " $ tProd [tVar "a",tVar "b",tVar "c",tVar "d",tVar "e",tRat (-1)],
  testEval "2*3" $ tRat 6,
  testEval "3*2" $ tRat 6,
  testEval "1*x" $ tVar "x",
  testEval "0*x" $ tRat 0,
  testEval "a+a" $ tProd [tVar "a",tRat 2],
  testEval "a+2*a" $ tProd [tVar "a",tRat 3],
  testEval "2*a+2*a" $ tProd [tVar "a",tRat 4],
  testEval "3*a+5*a" $ tProd [tVar "a",tRat 8],
  testEval "c+a+b+c+b+a+b+b+c+c+c+a+b+a+c" $ tSum [tProd [tVar "a",tRat 4],
                                                   tProd [tVar "b",tRat 5],
                                                   tProd [tVar "c",tRat 6]],
  testEval "2*c+5*a+7*b+6*c+3*b+9*a+4*b+5*b+3*c+8*c+7*c+2*a+4*b+6*a+6*c" $
  tSum [tProd [tVar "a",tRat 22],
        tProd [tVar "b",tRat 23],
        tProd [tVar "c",tRat 32]],
  testEval "(a+1)*(b+2)*c*(d+3)+(d+3)*5*(b+2)*c*(a+1)+4*(d+3)*c*(b+2)*(a+1)" $
  tProd [tVar "c",
         tSum [tVar "a",tRat 1],
         tSum [tVar "b",tRat 2],
         tSum [tVar "d",tRat 3],
         tRat 10],
  testEval "x^2" $ tIntPow (tVar "x") 2,
  testEval "1/x" $ tIntPow (tVar "x") (-1),
  testEval "f()" $ tCall "f" [],
  testEval "f(x)" $ tCall "f" [tVar "x"],
  testEval "f(x,y)" $ tCall "f" [tVar "x", tVar "y"],
  testEval "f()*2" $ tProd [tCall "f" [],tRat 2],
  testEval "f()*x" $ tProd [tVar "x",tCall "f" []],
  testEval "f()^2*x" $ tProd [tVar "x",tIntPow (tCall "f" []) 2],
  testEval "f()+2" $ tSum [tCall "f" [],tRat 2],
  testEval "f()+x" $ tSum [tVar "x",tCall "f" []],
  testEval "f()^2+x" $ tSum [tVar "x",tIntPow (tCall "f" []) 2],
  testEval "(x+1)*f()" $ tProd [tCall "f" [],tSum [tVar "x",tRat 1]],
  testEval "(x+1)*f()^2" $
    tProd [tIntPow (tCall "f" []) 2,tSum [tVar "x",tRat 1]]
  ]

testDisplay :: String -> String -> Test
testDisplay input output = astDisplay
  (fromExpr $ runBuiltins $ runSubstitute $ unRight $
   parseAll expressionParser $ map snd $ unRight $
   parseAll tokenizer input) ~?= output

expressionDisplayTests :: Test
expressionDisplayTests = test [
  testDisplay "3" "3",
  testDisplay "4" "4",
  testDisplay "-5" "-5",
  testDisplay "-8" "-8",
  testDisplay "x" "x",
  testDisplay "foo" "foo",
  testDisplay "ABCdef123" "ABCdef123",
  testDisplay "a+1" "a + 1",
  testDisplay "a+b" "a + b",
  testDisplay "b+a" "a + b",
  testDisplay "a*b" "a b",
  testDisplay "x*y" "x y",
  testDisplay "2*b" "2 b",
  testDisplay "b*2" "2 b",
  testDisplay "a*b+2*b" "a b + 2 b",
  testDisplay "a*(b+c)" "a (b + c)",
  testDisplay "(a+b)*(c+d)" "(a + b) (c + d)",
  testDisplay "(a+b)*c" "c (a + b)",
  testDisplay "(a+b)*(c+d)*(e+1)" "(a + b) (c + d) (e + 1)",
  testDisplay "a+1+1" "a + 2",
  testDisplay "3+7+c+b+a+5+d+12" "a + b + c + d + 27",
  testDisplay "a+0" "a",
  testDisplay "0" "0",
  testDisplay "0+0" "0",
  testDisplay "(0)" "0",
  testDisplay "(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0" "0",
  testDisplay "(1+1)+(1+(1+1)+(1+1+(1+1))+1)+1" "11",
  testDisplay "(x+1)+y" "x + y + 1",
  testDisplay "(x+1)+y+1" "x + y + 2",
  testDisplay "x*2" $ "2 x",
  testDisplay "x*(a+1)" "x (a + 1)",
  testDisplay "x*(a+b)" "x (a + b)",
  testDisplay "x*(b+a)" "x (a + b)",
  testDisplay "x*(a+1+1)" "x (a + 2)",
  testDisplay "x*(3+7+c+b+a+5+d+12)" "x (a + b + c + d + 27)",
  testDisplay "x*(a+b+0)" "x (a + b)",
  testDisplay "x*(a+b+0+0)" "x (a + b)",
  testDisplay "x*(a+b+(0+0)+(0+(0+0)+(0+0+(0+0))+0)+0)" "x (a + b)",
  testDisplay "x*(a+(b+c))" "x (a + b + c)",
  testDisplay "a*b+c*d" "a b + c d",
  testDisplay "x*y+z" "x y + z",
  testDisplay "z+x*y" "x y + z",
  testDisplay "x*y+a" "a + x y",
  testDisplay "a+x*y" "a + x y",
  testDisplay "a*z+x" "a z + x",
  testDisplay "x+a*z" "a z + x",
  testDisplay "a*b+2*c" "a b + 2 c",
  testDisplay "c*2+a*b" "a b + 2 c",
  testDisplay "a*c+2*b" "a c + 2 b",
  testDisplay "b*2+a*c" "a c + 2 b",
  testDisplay "b*c+a*2" "2 a + b c",
  testDisplay "2*a+b*c" "2 a + b c",
  testDisplay "x*z+y*z+x*y+x*y*z" "x y z + x y + x z + y z",
  testDisplay "z+y+x+x*z+y*z+x*y+x*y*z" "x y z + x y + x z + x + y z + y + z",
  testDisplay "1+z+y+x+x*z+y*z+x*y+x*y*z" "x y z + x y + x z + x + y z + y + z + 1",
  testDisplay "a*(y+1)+b*(x+1)" "a (y + 1) + b (x + 1)",
  testDisplay "b*(x+1)+a*(y+1)" "a (y + 1) + b (x + 1)",
  testDisplay "a*b+a*2" "a b + 2 a",
  testDisplay "a*(b*c)" "a b c",
  testDisplay "a+b*(c*d)" "a + b c d",
  testDisplay "b*a" "a b",
  testDisplay "a*(b+d*c)" "a (b + c d)",
  testDisplay "a+b*(d+c)" "a + b (c + d)",
  testDisplay "1*1" "1",
  testDisplay "1*1*1*(1*1*(1*(1*1)*1)*(1*1))*1" "1",
  testDisplay "(-1)*(-1)*(-1)*((-1)*(-1)*((-1)*((-1)*(-1))*(-1))*((-1)*(-1)))*(-1)" "1",
  testDisplay "a*(-1)*b*((-1)*c*((-1)*(d*(-1))*(-1))*((-1)*e))*(-1) " "-a b c d e",
  testDisplay "2*3" "6",
  testDisplay "3*2" "6",
  testDisplay "1*x" "x",
  testDisplay "0*x" "0",
  testDisplay "a+a" "2 a",
  testDisplay "a+2*a" "3 a",
  testDisplay "2*a+2*a" "4 a",
  testDisplay "3*a+5*a" "8 a",
  testDisplay "c+a+b+c+b+a+b+b+c+c+c+a+b+a+c" "4 a + 5 b + 6 c",
  testDisplay "2*c+5*a+7*b+6*c+3*b+9*a+4*b+5*b+3*c+8*c+7*c+2*a+4*b+6*a+6*c" "22 a + 23 b + 32 c",
  testDisplay "(a+1)*(b+2)*c*(d+3)+(d+3)*5*(b+2)*c*(a+1)+4*(d+3)*c*(b+2)*(a+1)" "10 c (a + 1) (b + 2) (d + 3)",
  testDisplay "-a" "-a",
  testDisplay "-(a)" "-a",
  testDisplay "(-a)" "-a",
  testDisplay "-(-a)" "a",
  testDisplay "-(5)" "-5",
  testDisplay "(-5)" "-5",
  testDisplay "-(-5)" "5",
  testDisplay "-1*x" "-x",
  testDisplay "1*-x" "-x",
  testDisplay "-1*-x" "x",
  testDisplay "1*x" "x",
  testDisplay "-2*x" "-2 x",
  testDisplay "2*-x" "-2 x",
  testDisplay "-2*-x" "2 x",
  testDisplay "2*x" "2 x",
  testDisplay "x-(-1)" "x + 1",
  testDisplay "x-1" "x - 1",
  testDisplay "-x-1" "-x - 1",
  testDisplay "x+y+1" "x + y + 1",
  testDisplay "1*x+1*y+1" "x + y + 1",
  testDisplay "2*x+2*y+2" "2 x + 2 y + 2",
  testDisplay "-x-y-1" "-x - y - 1",
  testDisplay "-1*x-1*y-1" "-x - y - 1",
  testDisplay "-2*x-2*y-2" "-2 x - 2 y - 2",
  testDisplay "x^2" "x^2",
  testDisplay "x^2+y^2" "x^2 + y^2",
  testDisplay "x^2+1" "x^2 + 1",
  testDisplay "1+x^2" "x^2 + 1",
  testDisplay "x^2+y" "x^2 + y",
  testDisplay "a*b+c^2" "a b + c^2",
  testDisplay "2*y^2" "2 y^2",
  testDisplay "x*y^2" "x y^2",
  testDisplay "(a+b)*x^2" "x^2 (a + b)",
  testDisplay "x^2+x" "x^2 + x",
  testDisplay "x^3+x^2" "x^3 + x^2",
  testDisplay "(x*y)^2" "x^2 y^2",
  testDisplay "2^2" "4",
  testDisplay "(x^2)^2" "x^4",
  testDisplay "(x^3)^3" "x^9",
  --testDisplay "x^(3^3)" "x^27" -- parse fails
  testDisplay "x^-1" "1 / x",
  testDisplay "x^-2" "1 / x^2",
  testDisplay "2^-2" "1 / 4",
  testDisplay "(2^-1)^-1" "2",
  testDisplay "x*x" "x^2",
  testDisplay "x^1" "x",
  testDisplay "x*x^2" "x^3",
  testDisplay "x^2*x^2" "x^4",
  testDisplay "2^3*3^2*5*7^2" "17640",
  testDisplay "2^-3*3^-2*5^-1*7^-2" "1 / 17640",
  testDisplay "2*3^-1*5" "10 / 3",
  testDisplay "2*3^-1*5^-1*7" "14 / 15",
  testDisplay "2^-1*x^-1" "1 / 2 x",
  testDisplay "1/2" "1 / 2",
  testDisplay "3/6" "1 / 2",
  testDisplay "1/x" "1 / x",
  testDisplay "1/x^2" "1 / x^2",
  testDisplay "x/y" "x / y",
  testDisplay "1/x/y" "1 / x y",
  testDisplay "y/x" "y / x",
  testDisplay "1/(1/x)" "x",
  testDisplay "(x^-1)^-1" "x",
  testDisplay "x^1" "x",
  testDisplay "2/3*x/y" "2 x / 3 y",
  testDisplay "2/3*y/x" "2 y / 3 x",
  testDisplay "2*x/y" "2 x / y",
  testDisplay "1/3*x/y" "x / 3 y",
  testDisplay "2/3*x" "2 x / 3",
  testDisplay "2/3/y" "2 / 3 y",
  testDisplay "2/3" "2 / 3",
  testDisplay "2*x" "2 x",
  testDisplay "2/y" "2 / y",
  testDisplay "1/3*x" "x / 3",
  testDisplay "1/3/y" "1 / 3 y",
  testDisplay "x/y" "x / y",
  testDisplay "2/3/5/7/11" "2 / 1155",
  testDisplay "(2/3)/(5/7)" "14 / 15",
  testDisplay "((2/3)/(5/7))/((11/13)/(17/19))" "3094 / 3135",
  testDisplay "((a/b)/(c/d))/((e/f)/(g/h))" "a d f g / b c e h",
  testDisplay "((a*b)/(c*d))/((e*f)/(g*h))" "a b g h / c d e f",
  testDisplay "((a/b)*(c/d))/((e/f)*(g/h))" "a c f h / b d e g",
  testDisplay "((a*b)*(c*d))/((e*f)*(g*h))" "a b c d / e f g h",
  testDisplay "((a/b)/(c/d))*((e/f)/(g/h))" "a d e h / b c f g",
  testDisplay "((a*b)/(c*d))*((e*f)/(g*h))" "a b e f / c d g h",
  testDisplay "((a/b)*(c/d))*((e/f)*(g/h))" "a c e g / b d f h",
  testDisplay "2/4" "1 / 2",
  testDisplay "1/2+1/2" "1",
  testDisplay "1/x+1/x" "2 / x",
  testDisplay "x/2+x/2" "x",
  testDisplay "1/x*1/x" "1 / x^2",
  testDisplay "1/(2*x)*1/(x*2)" "1 / 4 x^2",
  testDisplay "1/(2*x)*x*2" "1",
  testDisplay "expand(0)" "0",
  testDisplay "expand(5/3)" "5 / 3",
  testDisplay "expand(x)" "x",
  testDisplay "expand((a+b)*(u+v)*(x+y+3*z))" "a u x + a u y + 3 a u z + a v x + a v y + 3 a v z + b u x + b u y + 3 b u z + b v x + b v y + 3 b v z",
  testDisplay "expand(4/5*a^7*b^-2*(x+y))" "(4 a^7 x + 4 a^7 y) / 5 b^2",
  -- There is a sort order problem here
  -- Need to separate this into 2 classes of tests:
  -- 1. tests that two expressions evaluate and display as the same thing
  -- 2. tests that a certain expression displays in a certain form
  testDisplay "expand((1+x)^5)" "x^5 + 5 x^4 + 10 x^3 + 10 x^2 + 5 x + 1",
  testDisplay "expand((x+y+1)^3)" "x^3 + 3 x^2 y + 3 x^2 + 3 x y^2 + 6 x y + 3 x + y^3 + 3 y^2 + 3 y + 1",
  testDisplay "x^2+x*y" "x^2 + x y",
  testDisplay "(a+b)^2*z" "z (a + b)^2",
  testDisplay "z*(a+b)^2" "z (a + b)^2",
  testDisplay "(a+b)*(a+c)^2" "(a + b) (a + c)^2",
  testDisplay "(a+b)^2*(a+c)" "(a + b)^2 (a + c)",
  testDisplay "(a+c)*(a+b)^2" "(a + b)^2 (a + c)",
  testDisplay "(a+c)^2*(a+b)" "(a + b) (a + c)^2",
  testDisplay "a*b+(c+d)^2" "a b + (c + d)^2",
  testDisplay "c*d+(a+b)^2" "c d + (a + b)^2",
  testDisplay "(a+b)^2+c*d" "c d + (a + b)^2",
  testDisplay "(a+b)*(y+z)^2" "(a + b) (y + z)^2",
  testDisplay "(y+z)*(a+b)^2" "(a + b)^2 (y + z)",
  testDisplay "(a+b+1)*(y+z)^2" "(a + b + 1) (y + z)^2",
  testDisplay "(a+b)*(y+z+1)^2" "(a + b) (y + z + 1)^2",
  testDisplay "(a+b+1)^2*(y+z)" "(a + b + 1)^2 (y + z)",
  testDisplay "(a+b)^2*(y+z+1)" "(a + b)^2 (y + z + 1)",
  testDisplay "(y+z)^2*(a+b+1)" "(a + b + 1) (y + z)^2",
  testDisplay "(y+z+1)^2*(a+b)" "(a + b) (y + z + 1)^2",
  testDisplay "(y+z)*(a+b+1)^2" "(a + b + 1)^2 (y + z)",
  testDisplay "(y+z+1)*(a+b)^2" "(a + b)^2 (y + z + 1)",
  testDisplay "(a+b)^100+z^2" "z^2 + (a + b)^100",
  testDisplay "z^2+(a+b)^100" "z^2 + (a + b)^100",
  testDisplay "(2*x)^2/4" "x^2",
  testDisplay "substitute(x,a,x^2)" "a^2",
  testDisplay "f(y+x)" "f(x + y)",
  testDisplay "f(x*y)+f(x)" "f(x y) + f(x)",
  testDisplay "f(x+y)+f(x)" "f(x + y) + f(x)",
  testDisplay "f(x^2)+f(x)" "f(x^2) + f(x)",
  testDisplay "f(x)+f(x^2)+f(x*y)+f(x+y)+f(1)+f(f(x))+f(f(x)^2)" "f(x^2) + f(x y) + f(x + y) + f(x) + f(f(x)^2) + f(f(x)) + f(1)",
  testDisplay "f(x)*f(x^2)*f(x*y)*f(x+y)*f(1)*f(f(x))*f(f(x)^2)" "f(x^2) f(x y) f(x + y) f(x) f(f(x)^2) f(f(x)) f(1)",
  -- TODO equal degree factorization
  -- testDisplay "factor(f(x^2-1)^2+1)" "f((x - 1) (x + 1))^2 + 1",
  testDisplay "factor(expand((x-1)^5))" "(x - 1)^5",
  testDisplay "factor(expand((x^2-2*x+1)^5))" "(x - 1)^10",
  testDisplay "factor(expand((x^2-4*x+4)^5))" "(x - 2)^10",
  testDisplay "factor(f(x^2-2*x+1))" "f((x - 1)^2)",
  testDisplay "factor(f(x^2-2*x+1)+1)" "f((x - 1)^2) + 1",
  testDisplay "factor(expand((x+1)^3*x^2*(x-1)))" "x^2 (x - 1) (x + 1)^3",
  testDisplay "factor(x*y-1)" "x y - 1",
  testDisplay "factor(f(x)^2+6*f(x)+9)" "(f(x) + 3)^2",
  testDisplay "factor(expand((3*x+2)^100))" "(3 x + 2)^100",
  testDisplay "factor(expand((4*x+6)^5))" "32 (2 x + 3)^5",
  testDisplay "factor(f(x^2+2*x+1)+f(y^2+2*y+1))"
    "f((x + 1)^2) + f((y + 1)^2)"
  ]

storeTests :: Test
storeTests = test [
  getValue "a" newStore ~?= Nothing,
  getValue "a" (setValue "a" (tRat 42) newStore) ~?= Just (tRat 42),
  getValue "b" (setValue "a" (tRat 42) newStore) ~?= Nothing,
  getValue "a" (setValue "a" (tVar "x") (setValue "a" (tRat 11) newStore)) ~?= Just (tVar "x"),
  getValue "a" (setValue "b" (tVar "x") (setValue "a" (tRat 11) newStore)) ~?= Just (tRat 11),
  getValue "a" (setValue "a" (tVar "x") (setValue "b" (tRat 11) newStore)) ~?= Just (tVar "x")
  ]

integrationTest :: String -> String -> Test
integrationTest input desiredOutput =
  let commands = unRight $ parseAll commandParser $ map snd $ unRight $
                 parseAll tokenizer input
  in snd (executeCommands newStore commands) ~?= desiredOutput
  where
    executeCommands store [] = (store,"")
    executeCommands store [c] = execute store c
    executeCommands store (c:cs) =
      let (store', output1) = execute store c
          (store'', output2) = executeCommands store' cs
      in (store'', output1 ++ ";" ++ output2)

integrationTests :: Test
integrationTests = test
  [ integrationTest "a:=1;a" "a := 1;1"
  , integrationTest "substitute(x,x+1,x^3)" "(x + 1)^3"
  , integrationTest "x:=1;substitute(x,y,x^2)" "x := 1;y^2"
  , integrationTest "y:=3;substitute(x,y,x^2)" "y := 3;9"
  , integrationTest "substitute(y,x+z,(x+y)^100/(x*y*z))"
    "(2 x + z)^100 / x z (x + z)"
  , integrationTest "a:=1;b:=2;c:=3;clear b;a+b+c"
    "a := 1;b := 2;c := 3;Removed definition of b.;b + 4"
  , integrationTest "(a+b-1)*(a+b)*(a+b+1)" "(a + b - 1) (a + b) (a + b + 1)"
  , integrationTest "(a+b)*(a+b+c)" "(a + b + c) (a + b)"
  ]

tests :: Test
tests = test ["Tokenizer" ~: test_Tokenizer,
              "ASTParser" ~: test_ASTParser,
              "ASTDisplay" ~: test_ASTDisplay,
              "expression parser" ~: expressionParserTests,
              "expression display" ~: expressionDisplayTests,
              "expression" ~: test_Expression,
              "store" ~: storeTests,
              "expand" ~: test_Expand,
              "together" ~: test_Together,
              "factor" ~: test_Factor,
              "integration" ~: integrationTests
             ]

main :: IO ()
main = runTestTT tests >> return ()
