module Expand (expand,
               test_Expand) where

import Expression
import Test.HUnit
import Data.Ratio

expand :: Expression -> Expression
expand =
  eMatch eRat eVar (eSum . map expand) (eSum . expandProduct . map expand) eIntPow

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
  expand (x) ~?= x,
  expand (eProd [x,eSum [y,z]]) ~?=
         (eSum [eProd [x,y],eProd [x,z]]),
  expand (eSum [eRat 1,eProd [x,eSum [y,z]]]) ~?=
         (eSum [eRat 1,eProd [x,y],eProd [x,z]]),
  -- b*(c+d*(e+f))
  expand (eProd [b,eSum [c,eProd [d,eSum [e,f]]]]) ~?=
  eSum [eProd [b,c],eProd [b,d,e],eProd [b,d,f]]
  ]
  where
    a = eVar "a"
    b = eVar "b"
    c = eVar "c"
    d = eVar "d"
    e = eVar "e"
    f = eVar "f"
    x = eVar "x"
    y = eVar "y"
    z = eVar "z"
