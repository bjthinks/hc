module Command (Command(..), builtinFunctions, builtinCommands, execute,
                runBuiltins, testSubstitute) where

import DisplayExpression
import Expression
import Store
import Substitute
import Expand
import Factor
import Together
import Help
import HCException
import Control.Exception

data Command = CommandAssign String Expression |
               CommandBlank |
               CommandClear [String] |
               CommandEval Expression |
               CommandExit |
               CommandHelp String

builtinFunctions :: [String]
builtinFunctions = ["expand", "factor", "substitute", "together"]

builtinCommands :: [String]
builtinCommands = ["clear", "help", "exit"]

execute :: Store -> Command -> (Store, String)
execute store (CommandAssign v e) =
  if v `elem` builtinFunctions
  then throw HCRedefineBuiltin
  else (setValue v ee store, displayAssignment v ee)
  where ee = runBuiltins $ doSubstitutions store [] e
execute store CommandBlank = (store, "")
execute store (CommandClear vs) = clearValues vs store
execute store (CommandEval e) =
  (incrementResult $ setValue v ee store, displayAssignment v ee)
  where
    v = nextResult store
    ee = runBuiltins $ doSubstitutions store [] e
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

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpression e
