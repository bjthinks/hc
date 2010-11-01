module ASTDisplay (astDisplay, test_ASTDisplay) where

import AST
import Test.HUnit

astDisplay :: ASTExpr -> String
astDisplay = fst . astDisplayPrec

astDisplayPrec :: ASTExpr -> (String, Int)
astDisplayPrec (ASTInteger n) = (show n, 10)
astDisplayPrec (ASTVariable v) = (v, 10)
astDisplayPrec (ASTSum x y)        = ((dp x 0) ++ " + " ++ (dp y 0), 0)
astDisplayPrec (ASTDifference x y) = ((dp x 0) ++ " - " ++ (dp y 1), 0)
astDisplayPrec (ASTProduct x y)    = ((dp x 1) ++ " "   ++ (dp y 1), 1)
{-
astDisplayWithPrecedence (ASTQuotient x y) = (astDisplayWithPrecedence x) ++ " / " ++ (astDisplayWithPrecedence y)
astDisplayWithPrecedence (ASTPower x y) = (astDisplayWithPrecedence x) ++ "^" ++ (astDisplayWithPrecedence y)
astDisplayWithPrecedence (ASTNegation x) = "-" ++ (astDisplayWithPrecedence x)
astDisplayWithPrecedence (ASTCall f []) = f ++ "()"
astDisplayWithPrecedence (ASTCall f (a:as)) = f ++ "(" ++ (astDisplayWithPrecedence a) ++
                                concat (map (", " ++) (map astDisplayWithPrecedence as)) ++
                                ")"
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
