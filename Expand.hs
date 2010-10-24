module Expand (expand,
               test_Expand) where

import Expression
import Test.HUnit
import Data.Ratio ((%))
import Data.List (genericReplicate)

expand :: Expression -> Expression
expand = eTransform eRat eVar eSum expandProduct expandIntPow

expandProduct :: [Expression] -> Expression
expandProduct [] = eRat 1
expandProduct (x:xs) = eSum $
  [eProd [xTerm,restTerm] | xTerm <- xAsSum, restTerm <- xsAsSum]
  where
    xAsSum :: [Expression]
    xAsSum = eAsSum x
    xsAsSum :: [Expression]
    xsAsSum = eAsSum $ expandProduct xs

expandIntPow :: Expression -> Integer -> Expression
expandIntPow e n
  | n < 0 = eIntPow (expandIntPow e (-n)) (-1)
  | otherwise = expandProduct $ genericReplicate n e

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
  eSum [eProd [b,c],eProd [b,d,e],eProd [b,d,f]],
  expand (eIntPow (eSum [x,y]) 2) ~?=
  eSum [eIntPow x 2,eProd [eRat 2,x,y],eIntPow y 2],
  -- (y+z*(a+b))^2 -> 2 y z a + 2 y z b + y^2 + a^2 z^2 + 2 a b z^2 + b^2 z^2
  expand (eIntPow (eSum [y,eProd [z,eSum [a,b]]]) 2) ~?=
  eSum [eProd [eRat 2,y,z,a],
        eProd [eRat 2,y,z,b],
        eIntPow y 2,
        eProd [eIntPow a 2,eIntPow z 2],
        eProd [eRat 2,a,b,eIntPow z 2],
        eProd [eIntPow b 2,eIntPow z 2]]
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
