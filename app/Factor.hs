module Factor (factor, test_Factor) where

import Expression
import Factor.Defs
import qualified Factor.Tests as F
import Test.HUnit
import System.IO.Unsafe
import Data.Ratio

factor :: Expression -> Expression
factor = eTransform eRat eVar factorSum eProd eIntPow eCall

factorSum :: [Expression] -> Expression
factorSum es = unsafePerformIO (print $ map asTerm es) `seq` eSum es

asTerm :: Expression -> Maybe (Maybe Expression, Term)
asTerm = eMatch asTermRational asTermVariable undefined asTermProduct
  asTermIntPow asTermCall

asTermVariable :: String -> Maybe (Maybe Expression, Term)
asTermVariable v = Just (Just (eVar v),Term 1 1)

asTermIntPow :: Expression -> Integer -> Maybe (Maybe Expression, Term)
asTermIntPow e n = Just (Just e,Term 1 n)

asTermCall :: String -> [Expression] -> Maybe (Maybe Expression, Term)
asTermCall f es = Just (Just (eCall f es),Term 1 1)

asTermRational :: Rational -> Maybe (Maybe Expression, Term)
asTermRational i
  | denominator i == 1 = Just (Nothing,Term (numerator i) 0)
  | otherwise = Nothing

asTermProduct :: [Expression] -> Maybe (Maybe Expression, Term)
asTermProduct es = unsafePerformIO (print $ map asTermFactor es) `seq` Nothing
  where
    asTermFactor = eMatch asTermRational asTermVariable asTermSum undefined
      asTermIntPow asTermCall

asTermSum :: [Expression] -> Maybe (Maybe Expression, Term)
asTermSum es = Just (Just (eSum es), Term 1 1)

test_Factor :: Test
test_Factor = test
  [ F.tests
  , factor (eSum [x,o]) ~?= eSum [x,o]
  ]
  where
    o = eRat 1
    x = eVar "x"
