module ASTDisplay (astDisplay, test_ASTDisplay) where

import AST
import Test.HUnit

astDisplay :: ASTExpr -> String
astDisplay = fst . astDisplayPrec

astDisplayPrec :: ASTExpr -> (String, Int)
astDisplayPrec (ASTInteger n) = (show n, 10)
astDisplayPrec (ASTVariable v) = (v, 10)
astDisplayPrec (ASTSum        x y) = ((dp x 0) ++ " + " ++ (dp y 0), 0)
astDisplayPrec (ASTDifference x y) = ((dp x 0) ++ " - " ++ (dp y 1), 0)
astDisplayPrec (ASTProduct    x y) = ((dp x 1) ++ " "   ++ (dp y 1), 1)
{-
astDisplayPrec (ASTQuotient   x y) = ((dp x ?) ++ " / " ++ (dp y ?), ?)
astDisplayPrec (ASTPower      x y) = ((dp x ?) ++ "^"   ++ (dp y ?), ?)
astDisplayPrec (ASTNegation x) = ("-" ++ (dp x ?), ?)
astDisplayPrec (ASTCall f []) = (f ++ "()", ?)
astDisplayPrec (ASTCall f (a:as)) =
  (f ++ "(" ++ dp a 0 ++ concat (map (", "++) $ map (flip dp 0) as) ++ ")", ?)
-}

parenthesize :: (String, Int) -> Int -> String
parenthesize (str, p) q
  | p < q = "(" ++ str ++ ")"
  | otherwise = str

dp :: ASTExpr -> Int -> String
dp x n = parenthesize (astDisplayPrec x) n

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
  astDisplay (ASTDifference (ASTProduct a b) c) ~?= "a b - c"
  ]
  where
    a = ASTVariable "a"
    b = ASTVariable "b"
    c = ASTVariable "c"
