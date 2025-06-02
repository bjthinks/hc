module Together (together, togetherNotCalls, test_Together) where

import Expression
import Test.HUnit

together :: Expression -> Expression
together = id

togetherNotCalls :: Expression -> Expression
togetherNotCalls = id

test_Together :: Test
test_Together = test
  [ together (eSum [x,o]) ~?= eSum [x, o]
  ]
  where
    o = eRat 1
    x = eVar "x"
