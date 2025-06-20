module Command (Command(..), builtinFunctions, builtinCommands, execute,
                runBuiltins, testSubstitute) where

import DisplayExpression
import Expression
import Store
import Expand
import Factor
import Together
import Help
import HCException
import Control.Exception

data Command = CommandAssign String Expression |
               CommandBlank |
               CommandClear String |
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
execute store (CommandClear v) = clearValue v store
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

doSubstitutions :: Store -> [String] -> Expression -> Expression
doSubstitutions store vars = eMatch eRat get substituteSum substituteProduct
  substituteIntPow (substituteCall store vars)
  where
    get :: String -> Expression
    get str =
      case getValue str store of
        Just expr -> case elem str vars of
          True -> throw HCAssignmentLoop
          False -> doSubstitutions store (str:vars) expr
        Nothing -> eVar str
    substituteSum es = eSum $ map (doSubstitutions store vars) es
    substituteProduct es = eProd $ map (doSubstitutions store vars) es
    substituteIntPow b e = eIntPow (doSubstitutions store vars b) e

substituteCall :: Store -> [String] -> String -> [Expression] -> Expression
substituteCall store vars f es
  | f == "substitute" =
    let (e1,e2,e3) = case length es of
          3 -> (es !! 0, es !! 1, es !! 2)
          _ -> throw $ HCWrongNumberOfParameters "substitute" 3
        v = eMatch no id no no (const no) (const no) e1
        no _ = throw HCSubstituteNotVariable
        store' = setValue v e2 store
    in doSubstitutions store' vars e3
  | otherwise = eCall f $ map (doSubstitutions store vars) es

testSubstitute :: Expression -> Expression
testSubstitute = doSubstitutions newStore []

displayAssignment :: String -> Expression -> String
displayAssignment v e = v ++ " := " ++ displayExpression e
