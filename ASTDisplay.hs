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
{-
astDisplayWithPrecedence (ASTProduct x y) = (astDisplayWithPrecedence x) ++ " " ++ (astDisplayWithPrecedence y)
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
  astDisplay (ASTVariable "a") ~?= "a",
  astDisplay (ASTSum (ASTVariable "a") (ASTVariable "b")) ~?= "a + b",
  astDisplay (ASTSum (ASTSum (ASTVariable "a") (ASTVariable "b"))
              (ASTVariable "c")) ~?= "a + b + c",
  astDisplay (ASTSum (ASTVariable "a") (ASTSum (ASTVariable "b")
                                        (ASTVariable "c"))) ~?= "a + b + c",
  astDisplay (ASTDifference (ASTVariable "a") (ASTVariable "b")) ~?= "a - b",
  astDisplay (ASTDifference (ASTDifference (ASTVariable "a")
                             (ASTVariable "b")) (ASTVariable "c")) ~?=
  "a - b - c",
  astDisplay (ASTDifference (ASTVariable "a")
              (ASTDifference (ASTVariable "b")
               (ASTVariable "c"))) ~?= "a - (b - c)"
  ]
