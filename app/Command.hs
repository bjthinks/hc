module Command (Command(..), builtinFunctions, builtinCommands, execute,
                runBuiltins, runSubstitute) where

import AST
import ExprFromAST
import ASTFromExpr
import ASTDisplay
import Expression
import Store
import Expand
import Factor
import Together
import Substitute
import Help
import HCException
import Control.Exception

data Command = CommandAssign String AST |
               CommandBlank |
               CommandClear String |
               CommandEval AST |
               CommandExit |
               CommandHelp String

builtinFunctions :: [String]
builtinFunctions = ["expand", "factor", "substitute", "together"]

builtinCommands :: [String]
builtinCommands = ["clear", "help", "exit"]

execute :: Store -> Command -> (Store, String)
execute store (CommandAssign v a) =
  if v `elem` builtinFunctions
  then throw HCRedefineBuiltin
  else (setValue v e store, displayAssignment v e)
  where e = runBuiltins $ runSubstitute $ fromAST a
execute store CommandBlank = (store, "")
execute store (CommandClear v) = clearValue v store
execute store (CommandEval a) =
  (store, astDisplay $ fromExpr $ runBuiltins $ substituteFromStore store [] $
          runSubstitute $ fromAST a)
execute _ CommandExit = throw HCExit
execute store (CommandHelp topic) = (store, {-TODO word wrap-} showHelp topic)

runBuiltins :: Expression -> Expression
runBuiltins = eTransform eRat eVar eSum eProd eIntPow runBuiltin

runBuiltin :: String -> [Expression] -> Expression
runBuiltin "expand" [x] = expand x
runBuiltin "expand" _ = throw $ HCWrongNumberOfParameters "expand" 1
runBuiltin "factor" [x] = factor x
runBuiltin "factor" _ = throw $ HCWrongNumberOfParameters "factor" 1
runBuiltin "together" [x] = together x
runBuiltin "together" _ = throw $ HCWrongNumberOfParameters "together" 1
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

substituteFromStore :: Store -> [String] -> Expression -> Expression
substituteFromStore store vars = eTransform eRat get eSum eProd
  eIntPow eCall
  where
    get :: String -> Expression
    get str =
      case getValue str store of
        Just expr -> case elem str vars of
          True -> throw HCAssignmentLoop
          False -> substituteFromStore store (str:vars) expr
        Nothing -> eVar str

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ astDisplay (fromExpr e)
