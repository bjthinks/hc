module Expand (expand,
               test_Expand) where

import Expression
import Test.HUnit
import Data.Ratio

expand :: Expression -> Expression
expand =
  eMatch eRat eVar (eSum . map expand) (eSum . expandProduct) eIntPow

expandProduct :: [Expression] -> [Expression]
expandProduct [] = [eRat 1]
expandProduct (x:xs) =
  [eProd [xTerm,restTerm] | xTerm <- xAsSum, restTerm <- expandProduct xs]
  where
    xAsSum :: [Expression]
    xAsSum = eAsSum x

-- Note: might want 0 -> [] instead of 0 -> [0]
eAsSum :: Expression -> [Expression]
eAsSum = eMatch (\c -> [eRat c]) (\v -> [eVar v]) id (\e -> [eProd e])
         (\e n -> [eIntPow e n])

test_Expand :: Test
test_Expand = test [
  expand (eRat 0) ~?= eRat 0,
  expand (eRat (5%3)) ~?= eRat (5%3),
  expand (eVar "x") ~?= eVar "x",
  expand (eProd [eVar "x",eSum [eVar "y",eVar "z"]]) ~?=
         (eSum [eProd [eVar "x",eVar "y"],eProd [eVar "x",eVar "z"]]),
  expand (eSum [eRat 1,eProd [eVar "x",eSum [eVar "y",eVar "z"]]]) ~?=
         (eSum [eRat 1,eProd [eVar "x",eVar "y"],eProd [eVar "x",eVar "z"]])
  ]
