module Expand (expand,
               test_Expand) where

import Expression
import Test.HUnit
import Data.Ratio

expand :: Expression -> Expression
expand = id

test_Expand :: Test
test_Expand = test [
  expand (eRat 0) ~?= eRat 0,
  expand (eRat (5%3)) ~?= eRat (5%3),
  expand (eVar "x") ~?= eVar "x"
  ]
