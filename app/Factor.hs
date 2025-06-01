module Factor (factor, test_Factor) where

import Expression
import qualified Factor.Tests as F
import Test.HUnit

factor :: Expression -> Expression
factor = eTransform eRat eVar factorSum eProd eIntPow eCall

factorSum :: [Expression] -> Expression
factorSum = eSum

test_Factor :: Test
test_Factor = test
  [ F.tests
  , factor (eSum [x,o]) ~?= eSum [x,o]
  ]
  where
    o = eRat 1
    x = eVar "x"
