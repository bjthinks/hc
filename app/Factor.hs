module Factor (factor, test_Factor) where

import Expression
import Expand
import Together
import Substitute
import Factor.Defs
import Factor.Polynomial
import Factor.SquareFree
import qualified Factor.Tests as F
import Test.HUnit
import Data.Ratio

factor :: Expression -> Expression
factor = eMatch eRat eVar factorSum factorProduct factorIntPow factorCall

factorSum :: [Expression] -> Expression
-- This doesn't expand call parameters, e.g. factor(f((x+1)^10)-1)
factorSum es = realFactor $ togetherNotCalls $ expandNotCalls $ eSum es

factorProduct :: [Expression] -> Expression
factorProduct es = eProd $ map factor es

factorIntPow :: Expression -> Integer -> Expression
factorIntPow e n = eIntPow (factor e) n

factorCall :: String -> [Expression] -> Expression
factorCall f es = eCall f $ map factor es

-- This is probably a sum, and it has definitely been expanded
realFactor :: Expression -> Expression
realFactor = eMatch eRat eVar realFactorSum factorProduct factorIntPow
  factorCall

realFactorSum :: [Expression] -> Expression
realFactorSum es =
  let asterms = asTermSum es
  in case asterms of
    Nothing -> eSum $ map factor es
    Just (e, ts) -> case e of
      Nothing -> eSum $ map factor es -- This is probably an impossible case
      Just ee ->
        let Factorization c ps = squareFree $ makePolynomial ts
        in eProd $ eRat (c % 1) :
           map (uncurry $ processPolynomialToPower (factor ee)) ps

asTermSum :: [Expression] -> Maybe (Maybe Expression, [Term])
asTermSum es = combine Nothing [] (map asTerm es)
  where
    combine :: Maybe Expression -> [Term] -> [Maybe (Maybe Expression, Term)] ->
      Maybe (Maybe Expression, [Term])
    combine me ts [] = Just (me, reverse ts)
    combine _ _ (Nothing:_) = Nothing
    combine me1 ts (Just (me2, t2):inputs) = do
      me <- reconcile me1 me2
      combine me (t2:ts) inputs

asTerm :: Expression -> Maybe (Maybe Expression, Term)
asTerm = eMatch asTermRational asTermVariable undefined asTermProduct
  asTermIntPow asTermCall

asTermRational :: Rational -> Maybe (Maybe Expression, Term)
asTermRational i
  | denominator i == 1 = Just (Nothing,Term (numerator i) 0)
  | otherwise = Nothing

asTermVariable :: String -> Maybe (Maybe Expression, Term)
asTermVariable v = Just (Just (eVar v),Term 1 1)

asTermProduct :: [Expression] -> Maybe (Maybe Expression, Term)
asTermProduct es = combineProductTerms $ map asTerm es

combineProductTerms :: [Maybe (Maybe Expression, Term)] ->
  Maybe (Maybe Expression, Term)
combineProductTerms [] = undefined
combineProductTerms (Nothing:_) = Nothing
combineProductTerms [x@(Just _)] = x
combineProductTerms (Just (me1, t1):pts) = do
  (me2,t2) <- combineProductTerms pts
  me3 <- reconcile me1 me2
  let Term c1 exp1 = t1
      Term c2 exp2 = t2
      t3 = Term (c1*c2) (exp1+exp2)
  return (me3, t3)

reconcile :: Maybe Expression -> Maybe Expression -> Maybe (Maybe Expression)
reconcile Nothing Nothing = Just Nothing
reconcile x Nothing = Just x
reconcile Nothing x = Just x
reconcile (Just x) (Just y)
  | x == y = Just (Just x)
  | otherwise = Nothing

asTermIntPow :: Expression -> Integer -> Maybe (Maybe Expression, Term)
asTermIntPow e n
  | n >= 0 = Just (Just e,Term 1 n)
  | otherwise = Nothing

asTermCall :: String -> [Expression] -> Maybe (Maybe Expression, Term)
asTermCall f es = Just (Just (eCall f es),Term 1 1)

processPolynomialToPower :: Expression -> Polynomial -> Integer -> Expression
processPolynomialToPower x (Polynomial ts) e =
  eIntPow (eSum $ map (processTerm x) ts) e

processTerm :: Expression -> Term -> Expression
processTerm x (Term c e) = eProd [eRat (c % 1),eIntPow x e]

test_Factor :: Test
test_Factor = test
  [ F.tests
  , factor (eSum [eIntPow x 2,eProd [eRat 2,x],o]) ~?= eIntPow (eSum [x,o]) 2
  ]
  where
    o = eRat 1
    x = eVar "x"
