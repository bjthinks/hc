module ASTDisplay (astDisplay) where

import AST

astDisplay :: ASTExpr -> String
astDisplay (ASTInteger n) = show n
astDisplay (ASTVariable v) = v
astDisplay (ASTSum x y) = (astDisplay x) ++ " + " ++ (astDisplay y)
astDisplay (ASTDifference x y) = (astDisplay x) ++ " - " ++ (astDisplay y)
astDisplay (ASTProduct x y) = (astDisplay x) ++ " " ++ (astDisplay y)
astDisplay (ASTQuotient x y) = (astDisplay x) ++ " / " ++ (astDisplay y)
astDisplay (ASTPower x y) = (astDisplay x) ++ "^" ++ (astDisplay y)
astDisplay (ASTNegation x) = "-" ++ (astDisplay x)
astDisplay (ASTCall f []) = f ++ "()"
astDisplay (ASTCall f (a:as)) = f ++ "(" ++ (astDisplay a) ++
                                concat (map (", " ++) (map astDisplay as)) ++
                                ")"
