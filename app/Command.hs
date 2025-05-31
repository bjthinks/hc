module Command (Command(..), builtinFunctions, execute,
                runBuiltins, runSubstitute) where

import AST
import ExprFromAST
import ASTFromExpr
import ASTDisplay
import Expression
import Store
import Expand
import qualified Substitute as S
import HCException
import Control.Exception

data Command = CommandAssign String ASTExpr |
               CommandClear String |
               CommandEval ASTExpr

builtinFunctions :: [String]
builtinFunctions = ["expand", "substitute"]

execute :: Store -> Command -> (Store, String)
execute store (CommandAssign v a) =
  if v `elem` builtinFunctions
  then throw HCRedefineBuiltin
  else (setValue v e store, displayAssignment v e)
  where e = fromAST a
execute store (CommandClear v) =
  (clearValue v store, "Removed definition of " ++ v ++ ".")
execute store (CommandEval a) =
  (store, astDisplay $ fromExpr $ runBuiltins $ substitute store $
          runSubstitute $ fromAST a)

runBuiltins :: Expression -> Expression
runBuiltins = eTransform eRat eVar eSum eProd eIntPow runBuiltin

runBuiltin :: String -> [Expression] -> Expression
runBuiltin "expand" [x] = expand x
runBuiltin "expand" _ = throw $ HCWrongNumberOfParameters "expand" 1
runBuiltin f es = eCall f es

runSubstitute :: Expression -> Expression
runSubstitute = eTransform eRat eVar eSum eProd eIntPow runSubstitute'

runSubstitute' :: String -> [Expression] -> Expression
runSubstitute' "substitute" [e1,e2,e3] =
  eMatch no go no no (const no) (const no) e1
  where
    no _ = throw HCSubstituteNotVariable
    go v = S.substitute v e2 e3
runSubstitute' "substitute" _ = throw $ HCWrongNumberOfParameters "substitute" 3
runSubstitute' f es = eCall f es

substitute :: Store -> Expression -> Expression
substitute store = eTransform eRat (get store) eSum eProd eIntPow eCall

get :: Store -> String -> Expression
get store str =
  case getValue str store of
    Just expr -> substitute store expr
    Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ astDisplay (fromExpr e)
