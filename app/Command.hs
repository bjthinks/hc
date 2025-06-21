module Command (Command(..), execute, runBuiltins, testSubstitute) where

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
import Data.List

data Command = CommandAssignVariable String Expression |
               CommandAssignFunction String [String] Expression |
               CommandBlank |
               CommandClear [String] |
               CommandEval Expression |
               CommandExit |
               CommandHelp String

execute :: Store -> Command -> (Store, String)
execute store (CommandAssignVariable v e) =
  if v `elem` builtinFunctions || v `elem` builtinCommands
  then throw HCReservedWord
  else (setValue v ee store, displayAssignVariable v ee)
  where ee = runBuiltins $ doSubstitutions store [] e
execute store (CommandAssignFunction f args e) =
  if hasDuplicate args
  then throw HCDuplicateParameter
  else if f `elem` builtinFunctions || f `elem` builtinCommands
  then throw HCReservedWord
  else ({-setFunction f args ee-} store, displayAssignFunction f args ee)
  where ee = runBuiltins $ doSubstitutions store' [] e
        store' = case args of
          [] -> store
          _ -> fst $ clearValues args store
        hasDuplicate xs = hasDuplicateSorted $ sort xs
        hasDuplicateSorted [] = False
        hasDuplicateSorted [_] = False
        hasDuplicateSorted (x:y:zs)
          | x == y = True
          | otherwise = hasDuplicateSorted (y:zs)
execute store CommandBlank = (store, "")
execute store (CommandClear vs) = clearValues vs store
execute store (CommandEval e) =
  (incrementResult $ setValue v ee store, displayAssignVariable v ee)
  where
    v = nextResult store
    ee = runBuiltins $ doSubstitutions store [] e
execute _ CommandExit = throw HCExit
execute store (CommandHelp topic) = (store, {-TODO word wrap-} showHelp topic)

runBuiltins :: Expression -> Expression
runBuiltins = eTransform eRat eVar eSum eProd eIntPow runBuiltin

-- substitute is not here because it needs to happen within the
-- doSubstitutions function
runBuiltin :: String -> [Expression] -> Expression
runBuiltin "expand" [x] = expand x
runBuiltin "expand" _ = throw $ HCWrongNumberOfParameters "expand" 1
runBuiltin "factor" [x] = factor x
runBuiltin "factor" _ = throw $ HCWrongNumberOfParameters "factor" 1
runBuiltin "together" [x] = together x
runBuiltin "together" _ = throw $ HCWrongNumberOfParameters "together" 1
runBuiltin f es = eCall f es

displayAssignVariable :: String -> Expression -> String
displayAssignVariable v e = v ++ " := " ++ displayExpression e

displayAssignFunction :: String -> [String] -> Expression -> String
displayAssignFunction f args e =
  f ++ "(" ++ prettyArgs ++ ") := " ++ displayExpression e
  where
    prettyArgs = intercalate ", " args
