module Command (Command(..), execute) where

import Expression
import ASTFromExpr
import ASTDisplay
import Store

data Command = CommandAssign String Expression |
               CommandEval Expression

execute :: Store -> Command -> (Store, String)
execute s (CommandAssign v e) = (setValue v e s, displayAssignment v e)
execute s (CommandEval e) = (s, (astDisplay . fromExpr) (substitute s e))

substitute :: Store -> Expression -> Expression
substitute s = eTransform eRat (get s) eSum eProd eIntPow eCall

get :: Store -> String -> Expression
get store str =
  case getValue str store of
    Just expr -> substitute store expr
    Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ astDisplay (fromExpr e)
