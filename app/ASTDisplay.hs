module ASTDisplay (astDisplay, test_ASTDisplay) where

import AST
import Test.HUnit

astDisplay :: AST -> String
astDisplay = fst . astDisplayPrec

astDisplayPrec :: AST -> (String, Int)
astDisplayPrec (ASTInteger n) = (show n, 10)
astDisplayPrec (ASTVariable v) = (v, 10)
astDisplayPrec (ASTSum        x y) = ((dp x 0) ++ " + " ++ (dp y 0), 0)
astDisplayPrec (ASTDifference x y) = ((dp x 0) ++ " - " ++ (dp y 1), 0)
astDisplayPrec (ASTProduct    x y) = ((dp x 2) ++ " "   ++ (dp y 2), 2)
astDisplayPrec (ASTQuotient   x y) = ((dp x 2) ++ " / " ++ (dp y 2), 1)
astDisplayPrec (ASTPower      x y) = ((dp x 4) ++ "^"   ++ (dp y 3), 3)
astDisplayPrec (ASTNegation x) = ("-" ++ (dp x 1), 1)
astDisplayPrec (ASTCall f []) = (f ++ "()", 4)
astDisplayPrec (ASTCall f (a:as)) =
  (f ++ "(" ++ dp a 0 ++ concat (map (", "++) $ map (flip dp 0) as) ++ ")", 4)

parenthesize :: (String, Int) -> Int -> String
parenthesize (str, p) q
  | p < q = "(" ++ str ++ ")"
  | otherwise = str

dp :: AST -> Int -> String
dp x n = parenthesize (astDisplayPrec x) n

test_ASTDisplay :: [Test]
test_ASTDisplay = [
  astDisplay (ASTInteger 3) ~?= "3",
  astDisplay a ~?= "a",
  astDisplay (ASTSum a b) ~?= "a + b",
  astDisplay (ASTSum (ASTSum a b) c) ~?= "a + b + c",
  astDisplay (ASTSum a (ASTSum b c)) ~?= "a + b + c",
  astDisplay (ASTDifference a b) ~?= "a - b",
  astDisplay (ASTDifference (ASTDifference a b) c) ~?= "a - b - c",
  astDisplay (ASTDifference a (ASTDifference b c)) ~?= "a - (b - c)",
  astDisplay (ASTProduct a b) ~?= "a b",
  astDisplay (ASTProduct a (ASTSum b c)) ~?= "a (b + c)",
  astDisplay (ASTProduct a (ASTDifference b c)) ~?= "a (b - c)",
  astDisplay (ASTProduct (ASTSum a b) c) ~?= "(a + b) c",
  astDisplay (ASTProduct (ASTDifference a b) c) ~?= "(a - b) c",
  astDisplay (ASTSum a (ASTProduct b c)) ~?= "a + b c",
  astDisplay (ASTDifference a (ASTProduct b c)) ~?= "a - b c",
  astDisplay (ASTSum (ASTProduct a b) c) ~?= "a b + c",
  astDisplay (ASTDifference (ASTProduct a b) c) ~?= "a b - c",
  astDisplay (ASTQuotient a b) ~?= "a / b",
  astDisplay (ASTQuotient (ASTProduct a b) (ASTProduct c d)) ~?= "a b / c d",
  astDisplay (ASTProduct (ASTQuotient a b) (ASTQuotient c d))
  ~?= "(a / b) (c / d)",
  astDisplay (ASTProduct (ASTProduct a b) (ASTProduct c d)) ~?= "a b c d",
  astDisplay (ASTQuotient (ASTQuotient a b) (ASTQuotient c d))
  ~?= "(a / b) / (c / d)",
  astDisplay (ASTSum (ASTProduct a b) (ASTProduct c d)) ~?= "a b + c d",
  astDisplay (ASTDifference (ASTProduct a b) (ASTProduct c d)) ~?= "a b - c d",
  astDisplay (ASTSum (ASTQuotient a b) (ASTQuotient c d)) ~?= "a / b + c / d",
  astDisplay (ASTDifference (ASTQuotient a b) (ASTQuotient c d))
  ~?= "a / b - c / d",
  astDisplay (ASTProduct (ASTSum a b) (ASTSum c d)) ~?= "(a + b) (c + d)",
  astDisplay (ASTProduct (ASTDifference a b) (ASTDifference c d))
  ~?= "(a - b) (c - d)",
  astDisplay (ASTQuotient (ASTSum a b) (ASTSum c d)) ~?= "(a + b) / (c + d)",
  astDisplay (ASTQuotient (ASTDifference a b) (ASTDifference c d))
  ~?= "(a - b) / (c - d)",
  astDisplay (ASTPower a b) ~?= "a^b",
  astDisplay (ASTPower a (ASTPower b c)) ~?= "a^b^c",
  astDisplay (ASTPower (ASTPower a b) c) ~?= "(a^b)^c",
  astDisplay (ASTProduct (ASTPower a b) (ASTPower c d)) ~?= "a^b c^d",
  astDisplay (ASTPower (ASTProduct a b) (ASTProduct c d)) ~?= "(a b)^(c d)",
  astDisplay (ASTSum a (ASTNegation b)) ~?= "a + -b",
  astDisplay (ASTSum (ASTNegation a) b) ~?= "-a + b",
  astDisplay (ASTDifference a (ASTNegation b)) ~?= "a - -b",
  astDisplay (ASTDifference (ASTNegation a) b) ~?= "-a - b",
  astDisplay (ASTProduct a (ASTNegation b)) ~?= "a (-b)",
  astDisplay (ASTProduct (ASTNegation a) b) ~?= "(-a) b",
  astDisplay (ASTQuotient a (ASTNegation b)) ~?= "a / (-b)",
  astDisplay (ASTQuotient (ASTNegation a) b) ~?= "(-a) / b",
  astDisplay (ASTPower a (ASTNegation b)) ~?= "a^(-b)",
  astDisplay (ASTPower (ASTNegation a) b) ~?= "(-a)^b",
  astDisplay (ASTNegation (ASTSum a b)) ~?= "-(a + b)",
  astDisplay (ASTNegation (ASTDifference a b)) ~?= "-(a - b)",
  astDisplay (ASTNegation (ASTProduct a b)) ~?= "-a b",
  astDisplay (ASTNegation (ASTQuotient a b)) ~?= "-a / b",
  astDisplay (ASTNegation (ASTPower a b)) ~?= "-a^b"
  ]
  where
    a = ASTVariable "a"
    b = ASTVariable "b"
    c = ASTVariable "c"
    d = ASTVariable "d"
