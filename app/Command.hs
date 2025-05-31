module Command (Command(..), builtinFunctions, builtinCommands, execute,
                runBuiltins, runSubstitute) where

import AST
import ExprFromAST
import ASTFromExpr
import ASTDisplay
import Expression
import Store
import Expand
import Substitute
import Help
import HCException
import Tokenizer
import Control.Exception

data Command = CommandAssign String ASTExpr |
               CommandClear String |
               CommandEval ASTExpr |
               CommandHelp (Maybe Token)

builtinFunctions :: [String]
builtinFunctions = ["expand", "substitute"]

builtinCommands :: [String]
builtinCommands = ["clear", "help"]

execute :: Store -> Command -> (Store, String)
execute store (CommandAssign v a) =
  if v `elem` builtinFunctions
  then throw HCRedefineBuiltin
  else (setValue v e store, displayAssignment v e)
  where e = fromAST a
execute store (CommandClear v) = clearValue v store
execute store (CommandEval a) =
  (store, astDisplay $ fromExpr $ runBuiltins $ substituteFromStore store $
          runSubstitute $ fromAST a)
execute store (CommandHelp topic) = (store, {-TODO word wrap-} showHelp topic)

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
    go v = substitute v e2 e3
runSubstitute' "substitute" _ = throw $ HCWrongNumberOfParameters "substitute" 3
runSubstitute' f es = eCall f es

substituteFromStore :: Store -> Expression -> Expression
substituteFromStore store = eTransform eRat get eSum eProd eIntPow eCall
  where
    get :: String -> Expression
    get str =
      case getValue str store of
        Just expr -> substituteFromStore store expr
        Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ astDisplay (fromExpr e)
