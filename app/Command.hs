module Command (Command(..), execute) where

import AST
import ExprFromAST
import ASTFromExpr
import ASTDisplay
import Expression
import Store

data Command = CommandAssign String ASTExpr |
               CommandClear String |
               CommandEval ASTExpr

execute :: Store -> Command -> (Store, String)
execute s (CommandAssign v a) = (setValue v e s, displayAssignment v e)
  where e = fromAST a
execute s (CommandClear v) =
  (clearValue v s, "Removed definition of " ++ v ++ ".")
execute s (CommandEval a) =
  (s, astDisplay $ fromExpr $ substitute s $ fromAST a)

substitute :: Store -> Expression -> Expression
substitute s = eTransform eRat (get s) eSum eProd eIntPow eCall

get :: Store -> String -> Expression
get store str =
  case getValue str store of
    Just expr -> substitute store expr
    Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ astDisplay (fromExpr e)
