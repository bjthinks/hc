module Substitute (doSubstitutions, testSubstitute) where

import Expression
import HCException
import Store
import Control.Exception

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
